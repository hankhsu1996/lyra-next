use std::collections::HashSet;

use lyra_ast::{AstNode, ErasedAstId};
use lyra_parser::SyntaxNode;
use lyra_semantic::const_eval::{ConstLookup, eval_const_expr};
use lyra_semantic::types::{ConstEvalError, ConstInt};
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

use crate::elaboration::{ElabDiag, ParamEnvId, ParamEnvInterner};
use crate::module_sig::{DesignUnitSig, ParamKind};
use crate::pipeline::{ast_id_map, parse_file};
use crate::{CompilationUnit, source_file_by_id};

pub(crate) struct ParamLookup<'a> {
    pub(crate) sig: &'a DesignUnitSig,
    pub(crate) values: &'a [ConstInt],
    pub(crate) prefix_len: usize,
}

impl ConstLookup for ParamLookup<'_> {
    fn lookup(&self, name: &str) -> Option<ConstInt> {
        let (idx, _) = self.sig.param_by_name(name)?;
        let i = idx as usize;
        if i < self.prefix_len {
            Some(self.values[i].clone())
        } else {
            Some(ConstInt::Error(ConstEvalError::Unresolved))
        }
    }
}

pub(crate) struct ParamOverride {
    pub(crate) name: Option<SmolStr>,
    pub(crate) value: ConstInt,
    pub(crate) span: TextRange,
}

/// Bundled context for expression evaluation in a scope.
pub(crate) struct EvalEnv<'a> {
    pub(crate) file_id: FileId,
    pub(crate) sig: &'a DesignUnitSig,
    pub(crate) param_env: ParamEnvId,
    pub(crate) genvar_values: &'a [(SmolStr, ConstInt)],
    pub(crate) interner: &'a ParamEnvInterner,
}

pub(crate) fn collect_overrides(
    sig: &DesignUnitSig,
    overrides: &[ParamOverride],
    file_id: FileId,
    inst_range: TextRange,
    diags: &mut Vec<ElabDiag>,
) -> Vec<Option<ConstInt>> {
    let param_count = sig.params.len();
    let mut result = vec![None; param_count];

    let has_named = overrides.iter().any(|o| o.name.is_some());
    if has_named {
        let mut seen: HashSet<u32> = HashSet::new();
        for ovr in overrides {
            let Some(ref name) = ovr.name else {
                continue;
            };
            match sig.param_by_name(name) {
                Some((idx, _)) => {
                    if !seen.insert(idx) {
                        diags.push(ElabDiag::DuplicateParamOverride {
                            name: SmolStr::new(name.as_str()),
                            span: Span {
                                file: file_id,
                                range: ovr.span,
                            },
                        });
                        continue;
                    }
                    if matches!(ovr.value, ConstInt::Error(_)) {
                        diags.push(ElabDiag::ParamNotConst {
                            name: sig.params[idx as usize].name.clone(),
                            span: Span {
                                file: file_id,
                                range: ovr.span,
                            },
                        });
                    }
                    result[idx as usize] = Some(ovr.value.clone());
                }
                None => {
                    diags.push(ElabDiag::UnknownParam {
                        name: SmolStr::new(name.as_str()),
                        module: sig.name.clone(),
                        span: Span {
                            file: file_id,
                            range: ovr.span,
                        },
                    });
                }
            }
        }
    } else {
        let value_param_count = sig
            .params
            .iter()
            .filter(|p| p.kind == ParamKind::Value)
            .count();
        if overrides.len() > value_param_count {
            diags.push(ElabDiag::TooManyPositionalParams {
                expected: value_param_count,
                got: overrides.len(),
                span: Span {
                    file: file_id,
                    range: inst_range,
                },
            });
        }
        let mut ovr_idx = 0;
        for (i, param) in sig.params.iter().enumerate() {
            if param.kind == ParamKind::Type {
                continue;
            }
            if ovr_idx < overrides.len() {
                if matches!(overrides[ovr_idx].value, ConstInt::Error(_)) {
                    diags.push(ElabDiag::ParamNotConst {
                        name: param.name.clone(),
                        span: Span {
                            file: file_id,
                            range: overrides[ovr_idx].span,
                        },
                    });
                }
                result[i] = Some(overrides[ovr_idx].value.clone());
                ovr_idx += 1;
            }
        }
    }

    result
}

pub(crate) fn build_param_env(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    sig: &DesignUnitSig,
    overrides: &[ParamOverride],
    inst_span: Span,
    diags: &mut Vec<ElabDiag>,
    interner: &mut ParamEnvInterner,
) -> ParamEnvId {
    let param_count = sig.params.len();
    if param_count == 0 {
        return ParamEnvId::EMPTY;
    }

    let override_map = collect_overrides(sig, overrides, inst_span.file, inst_span.range, diags);

    let def_file_id = sig
        .params
        .iter()
        .find_map(|p| p.default_expr.as_ref().map(|e| e.file()));
    let def_source_file = def_file_id.and_then(|fid| source_file_by_id(db, unit, fid));
    let id_map = def_source_file.map(|sf| ast_id_map(db, sf));
    let parse = def_source_file.map(|sf| parse_file(db, sf));

    let mut values = Vec::with_capacity(param_count);
    for (i, param) in sig.params.iter().enumerate() {
        if param.kind == ParamKind::Type {
            values.push(ConstInt::Error(ConstEvalError::Unsupported));
            continue;
        }

        if let Some(ref ovr_val) = override_map[i] {
            values.push(ovr_val.clone());
            continue;
        }

        let val = if let Some(expr_ast_id) = &param.default_expr {
            if let (Some(id_map), Some(parse)) = (&id_map, &parse) {
                if let Some(expr_node) = id_map.get_node(&parse.syntax(), *expr_ast_id) {
                    let param_lookup = ParamLookup {
                        sig,
                        values: &values,
                        prefix_len: i,
                    };
                    let resolve_name = |name_node: &SyntaxNode| -> Result<i64, ConstEvalError> {
                        let name_text = extract_name_text(name_node)?;
                        if let Some(ci) = param_lookup.lookup(&name_text) {
                            return match ci {
                                ConstInt::Known(v) => Ok(v),
                                ConstInt::Error(e) => Err(e),
                                ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
                            };
                        }
                        let name_ast_id = id_map
                            .erased_ast_id(name_node)
                            .ok_or(ConstEvalError::Unresolved)?;
                        let expr_ref = crate::const_eval::ConstExprRef::new(db, unit, name_ast_id);
                        match crate::const_eval::eval_const_int(db, expr_ref) {
                            ConstInt::Known(v) => Ok(v),
                            ConstInt::Error(e) => Err(e),
                            ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
                        }
                    };
                    match eval_const_expr(&expr_node, &resolve_name) {
                        Ok(v) => ConstInt::Known(v),
                        Err(e) => ConstInt::Error(e),
                    }
                } else {
                    let expr_ref = crate::const_eval::ConstExprRef::new(db, unit, *expr_ast_id);
                    crate::const_eval::eval_const_int(db, expr_ref)
                }
            } else {
                let expr_ref = crate::const_eval::ConstExprRef::new(db, unit, *expr_ast_id);
                crate::const_eval::eval_const_int(db, expr_ref)
            }
        } else {
            ConstInt::Error(ConstEvalError::Unresolved)
        };
        values.push(val);
    }

    interner.intern(values)
}

pub(crate) fn extract_param_overrides(
    inst_node: &lyra_ast::ModuleInstantiation,
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    env: &EvalEnv<'_>,
) -> Vec<ParamOverride> {
    let mut overrides = Vec::new();

    for ip in inst_node.param_override_ports() {
        let name = ip.port_name().map(|t| SmolStr::new(t.text()));
        let span = ip.syntax().text_range();

        let value = if let Some(expr_node) = ip.actual_expr() {
            match eval_env_expr(db, unit, &expr_node, env) {
                Ok(v) => ConstInt::Known(v),
                Err(e) => ConstInt::Error(e),
            }
        } else {
            ConstInt::Error(ConstEvalError::Unresolved)
        };

        overrides.push(ParamOverride { name, value, span });
    }

    overrides
}

pub(crate) fn eval_env_expr(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    expr_node: &SyntaxNode,
    env: &EvalEnv<'_>,
) -> Result<i64, ConstEvalError> {
    let source_file = source_file_by_id(db, unit, env.file_id).ok_or(ConstEvalError::Unresolved)?;
    let id_map = ast_id_map(db, source_file);

    if env.genvar_values.is_empty()
        && env.sig.params.is_empty()
        && let Some(expr_ast_id) = id_map.erased_ast_id(expr_node)
    {
        let expr_ref = crate::const_eval::ConstExprRef::new(db, unit, expr_ast_id);
        if let ConstInt::Known(v) = crate::const_eval::eval_const_int(db, expr_ref) {
            return Ok(v);
        }
    }

    let env_values = env.interner.values(env.param_env);
    let param_lookup = ParamLookup {
        sig: env.sig,
        values: env_values,
        prefix_len: env.sig.params.len(),
    };

    let resolve_name = |name_node: &SyntaxNode| -> Result<i64, ConstEvalError> {
        let name_text = extract_name_text(name_node)?;

        for (gn, gv) in env.genvar_values.iter().rev() {
            if name_text == gn.as_str() {
                return match gv {
                    ConstInt::Known(v) => Ok(*v),
                    ConstInt::Error(e) => Err(*e),
                    ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
                };
            }
        }

        if let Some(ci) = param_lookup.lookup(&name_text) {
            return match ci {
                ConstInt::Known(v) => Ok(v),
                ConstInt::Error(e) => Err(e),
                ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
            };
        }

        let name_ast_id = id_map
            .erased_ast_id(name_node)
            .ok_or(ConstEvalError::Unresolved)?;
        let expr_ref = crate::const_eval::ConstExprRef::new(db, unit, name_ast_id);
        match crate::const_eval::eval_const_int(db, expr_ref) {
            ConstInt::Known(v) => Ok(v),
            ConstInt::Error(e) => Err(e),
            ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
        }
    };

    eval_const_expr(expr_node, &resolve_name)
}

pub(crate) fn eval_gen_condition(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    expr_node: &SyntaxNode,
    env: &EvalEnv<'_>,
    cache: &mut std::collections::HashMap<crate::elaboration::CondCacheKey, ConstInt>,
    cache_key: Option<crate::elaboration::CondCacheKey>,
) -> Option<i64> {
    if let Some(ref key) = cache_key
        && let Some(cached) = cache.get(key)
    {
        return match cached {
            ConstInt::Known(v) => Some(*v),
            _ => None,
        };
    }

    let result = eval_env_expr(db, unit, expr_node, env);

    let const_int = match &result {
        Ok(v) => ConstInt::Known(*v),
        Err(e) => ConstInt::Error(*e),
    };

    if let Some(key) = cache_key {
        cache.insert(key, const_int);
    }

    result.ok()
}

pub(crate) fn extract_name_text(name_node: &SyntaxNode) -> Result<String, ConstEvalError> {
    use lyra_ast::AstNode;
    if let Some(name_ref) = lyra_ast::NameRef::cast(name_node.clone()) {
        return name_ref
            .ident()
            .map(|t| t.text().to_string())
            .ok_or(ConstEvalError::Unresolved);
    }
    if let Some(qn) = lyra_ast::QualifiedName::cast(name_node.clone()) {
        return qn
            .segments()
            .last()
            .map(|t| t.text().to_string())
            .ok_or(ConstEvalError::Unresolved);
    }
    Err(ConstEvalError::Unresolved)
}

pub(crate) fn cond_site_key(
    ast_id: ErasedAstId,
    kind: crate::elaboration::CondSiteKind,
    param_env: ParamEnvId,
    genvar_env: crate::elaboration::GenvarEnvId,
) -> crate::elaboration::CondCacheKey {
    (ast_id, kind, param_env, genvar_env)
}
