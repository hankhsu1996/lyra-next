use crate::SyntaxKind;

/// Classify an identifier string as a keyword or plain `Ident`.
///
/// Currently classifies against the full IEEE 1800-2023 keyword set.
/// The LRM (22.14) defines `` `begin_keywords ``/`` `end_keywords `` directives
/// that restrict the active keyword set by version (e.g. `"1364-2001"` only
/// reserves Verilog-2001 keywords). This lexer does not yet support version
/// gating -- all 1800-2023 keywords are always reserved. When
/// `begin_keywords` support is added, classification should accept a version
/// parameter and filter accordingly.
///
/// Uses length prefilter to skip irrelevant match arms.
pub(crate) fn classify_keyword(word: &str) -> SyntaxKind {
    match word.len() {
        2 => classify_2(word),
        3 => classify_3(word),
        4 => classify_4(word),
        5 => classify_5(word),
        6 => classify_6(word),
        7 => classify_7(word),
        8 => classify_8(word),
        9 => classify_9(word),
        10 => classify_10(word),
        11.. => classify_long(word),
        _ => SyntaxKind::Ident,
    }
}

fn classify_2(w: &str) -> SyntaxKind {
    match w {
        "do" => SyntaxKind::DoKw,
        "if" => SyntaxKind::IfKw,
        "or" => SyntaxKind::OrKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_3(w: &str) -> SyntaxKind {
    match w {
        "and" => SyntaxKind::AndKw,
        "bit" => SyntaxKind::BitKw,
        "buf" => SyntaxKind::BufKw,
        "end" => SyntaxKind::EndKw,
        "for" => SyntaxKind::ForKw,
        "iff" => SyntaxKind::IffKw,
        "int" => SyntaxKind::IntKw,
        "let" => SyntaxKind::LetKw,
        "new" => SyntaxKind::NewKw,
        "nor" => SyntaxKind::NorKw,
        "not" => SyntaxKind::NotKw,
        "ref" => SyntaxKind::RefKw,
        "reg" => SyntaxKind::RegKw,
        "tri" => SyntaxKind::TriKw,
        "use" => SyntaxKind::UseKw,
        "var" => SyntaxKind::VarKw,
        "wor" => SyntaxKind::WorKw,
        "xor" => SyntaxKind::XorKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_4(w: &str) -> SyntaxKind {
    match w {
        "bind" => SyntaxKind::BindKw,
        "bins" => SyntaxKind::BinsKw,
        "byte" => SyntaxKind::ByteKw,
        "case" => SyntaxKind::CaseKw,
        "cell" => SyntaxKind::CellKw,
        "cmos" => SyntaxKind::CmosKw,
        "dist" => SyntaxKind::DistKw,
        "edge" => SyntaxKind::EdgeKw,
        "else" => SyntaxKind::ElseKw,
        "enum" => SyntaxKind::EnumKw,
        "fork" => SyntaxKind::ForkKw,
        "join" => SyntaxKind::JoinKw,
        "nand" => SyntaxKind::NandKw,
        "nmos" => SyntaxKind::NmosKw,
        "null" => SyntaxKind::NullKw,
        "pmos" => SyntaxKind::PmosKw,
        "pure" => SyntaxKind::PureKw,
        "rand" => SyntaxKind::RandKw,
        "real" => SyntaxKind::RealKw,
        "soft" => SyntaxKind::SoftKw,
        "task" => SyntaxKind::TaskKw,
        "this" => SyntaxKind::ThisKw,
        "time" => SyntaxKind::TimeKw,
        "tran" => SyntaxKind::TranKw,
        "tri0" => SyntaxKind::Tri0Kw,
        "tri1" => SyntaxKind::Tri1Kw,
        "type" => SyntaxKind::TypeKw,
        "void" => SyntaxKind::VoidKw,
        "wait" => SyntaxKind::WaitKw,
        "wand" => SyntaxKind::WandKw,
        "weak" => SyntaxKind::WeakKw,
        "wire" => SyntaxKind::WireKw,
        "with" => SyntaxKind::WithKw,
        "xnor" => SyntaxKind::XnorKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_5(w: &str) -> SyntaxKind {
    match w {
        "alias" => SyntaxKind::AliasKw,
        "begin" => SyntaxKind::BeginKw,
        "break" => SyntaxKind::BreakKw,
        "casex" => SyntaxKind::CasexKw,
        "casez" => SyntaxKind::CasezKw,
        "class" => SyntaxKind::ClassKw,
        "const" => SyntaxKind::ConstKw,
        "cover" => SyntaxKind::CoverKw,
        "cross" => SyntaxKind::CrossKw,
        "event" => SyntaxKind::EventKw,
        "final" => SyntaxKind::FinalKw,
        "force" => SyntaxKind::ForceKw,
        "inout" => SyntaxKind::InoutKw,
        "input" => SyntaxKind::InputKw,
        "large" => SyntaxKind::LargeKw,
        "local" => SyntaxKind::LocalKw,
        "logic" => SyntaxKind::LogicKw,
        "pull0" => SyntaxKind::Pull0Kw,
        "pull1" => SyntaxKind::Pull1Kw,
        "randc" => SyntaxKind::RandcKw,
        "rcmos" => SyntaxKind::RcmosKw,
        "rnmos" => SyntaxKind::RnmosKw,
        "rpmos" => SyntaxKind::RpmosKw,
        "rtran" => SyntaxKind::RtranKw,
        "small" => SyntaxKind::SmallKw,
        "solve" => SyntaxKind::SolveKw,
        "super" => SyntaxKind::SuperKw,
        "table" => SyntaxKind::TableKw,
        "trior" => SyntaxKind::TriorKw,
        "union" => SyntaxKind::UnionKw,
        "until" => SyntaxKind::UntilKw,
        "uwire" => SyntaxKind::UwireKw,
        "weak0" => SyntaxKind::Weak0Kw,
        "weak1" => SyntaxKind::Weak1Kw,
        "while" => SyntaxKind::WhileKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_6(w: &str) -> SyntaxKind {
    match w {
        "always" => SyntaxKind::AlwaysKw,
        "assert" => SyntaxKind::AssertKw,
        "assign" => SyntaxKind::AssignKw,
        "assume" => SyntaxKind::AssumeKw,
        "before" => SyntaxKind::BeforeKw,
        "binsof" => SyntaxKind::BinsofKw,
        "bufif0" => SyntaxKind::Bufif0Kw,
        "bufif1" => SyntaxKind::Bufif1Kw,
        "config" => SyntaxKind::ConfigKw,
        "design" => SyntaxKind::DesignKw,
        "expect" => SyntaxKind::ExpectKw,
        "export" => SyntaxKind::ExportKw,
        "extern" => SyntaxKind::ExternKw,
        "genvar" => SyntaxKind::GenvarKw,
        "global" => SyntaxKind::GlobalKw,
        "highz0" => SyntaxKind::Highz0Kw,
        "highz1" => SyntaxKind::Highz1Kw,
        "ifnone" => SyntaxKind::IfnoneKw,
        "import" => SyntaxKind::ImportKw,
        "incdir" => SyntaxKind::IncdirKw,
        "inside" => SyntaxKind::InsideKw,
        "medium" => SyntaxKind::MediumKw,
        "module" => SyntaxKind::ModuleKw,
        "notif0" => SyntaxKind::Notif0Kw,
        "notif1" => SyntaxKind::Notif1Kw,
        "output" => SyntaxKind::OutputKw,
        "packed" => SyntaxKind::PackedKw,
        "pullup" => SyntaxKind::PullupKw,
        "repeat" => SyntaxKind::RepeatKw,
        "return" => SyntaxKind::ReturnKw,
        "signed" => SyntaxKind::SignedKw,
        "static" => SyntaxKind::StaticKw,
        "string" => SyntaxKind::StringKw,
        "strong" => SyntaxKind::StrongKw,
        "struct" => SyntaxKind::StructKw,
        "tagged" => SyntaxKind::TaggedKw,
        "triand" => SyntaxKind::TriandKw,
        "trireg" => SyntaxKind::TriregKw,
        "unique" => SyntaxKind::UniqueKw,
        "within" => SyntaxKind::WithinKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_7(w: &str) -> SyntaxKind {
    match w {
        "chandle" => SyntaxKind::ChandleKw,
        "checker" => SyntaxKind::CheckerKw,
        "context" => SyntaxKind::ContextKw,
        "default" => SyntaxKind::DefaultKw,
        "disable" => SyntaxKind::DisableKw,
        "endcase" => SyntaxKind::EndcaseKw,
        "endtask" => SyntaxKind::EndtaskKw,
        "extends" => SyntaxKind::ExtendsKw,
        "foreach" => SyntaxKind::ForeachKw,
        "forever" => SyntaxKind::ForeverKw,
        "implies" => SyntaxKind::ImpliesKw,
        "include" => SyntaxKind::IncludeKw,
        "initial" => SyntaxKind::InitialKw,
        "integer" => SyntaxKind::IntegerKw,
        "liblist" => SyntaxKind::LiblistKw,
        "library" => SyntaxKind::LibraryKw,
        "longint" => SyntaxKind::LongintKw,
        "matches" => SyntaxKind::MatchesKw,
        "modport" => SyntaxKind::ModportKw,
        "negedge" => SyntaxKind::NegedgeKw,
        "nettype" => SyntaxKind::NettypeKw,
        "package" => SyntaxKind::PackageKw,
        "posedge" => SyntaxKind::PosedgeKw,
        "program" => SyntaxKind::ProgramKw,
        "release" => SyntaxKind::ReleaseKw,
        "s_until" => SyntaxKind::SUntilKw,
        "specify" => SyntaxKind::SpecifyKw,
        "strong0" => SyntaxKind::Strong0Kw,
        "strong1" => SyntaxKind::Strong1Kw,
        "supply0" => SyntaxKind::Supply0Kw,
        "supply1" => SyntaxKind::Supply1Kw,
        "tranif0" => SyntaxKind::Tranif0Kw,
        "tranif1" => SyntaxKind::Tranif1Kw,
        "typedef" => SyntaxKind::TypedefKw,
        "unique0" => SyntaxKind::Unique0Kw,
        "untyped" => SyntaxKind::UntypedKw,
        "virtual" => SyntaxKind::VirtualKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_8(w: &str) -> SyntaxKind {
    match w {
        "clocking" => SyntaxKind::ClockingKw,
        "continue" => SyntaxKind::ContinueKw,
        "deassign" => SyntaxKind::DeassignKw,
        "defparam" => SyntaxKind::DefparamKw,
        "endclass" => SyntaxKind::EndclassKw,
        "endgroup" => SyntaxKind::EndgroupKw,
        "endtable" => SyntaxKind::EndtableKw,
        "forkjoin" => SyntaxKind::ForkjoinKw,
        "function" => SyntaxKind::FunctionKw,
        "generate" => SyntaxKind::GenerateKw,
        "instance" => SyntaxKind::InstanceKw,
        "join_any" => SyntaxKind::JoinAnyKw,
        "nexttime" => SyntaxKind::NexttimeKw,
        "priority" => SyntaxKind::PriorityKw,
        "property" => SyntaxKind::PropertyKw,
        "pulldown" => SyntaxKind::PulldownKw,
        "randcase" => SyntaxKind::RandcaseKw,
        "realtime" => SyntaxKind::RealtimeKw,
        "restrict" => SyntaxKind::RestrictKw,
        "rtranif0" => SyntaxKind::Rtranif0Kw,
        "rtranif1" => SyntaxKind::Rtranif1Kw,
        "s_always" => SyntaxKind::SAlwaysKw,
        "scalared" => SyntaxKind::ScalaredKw,
        "sequence" => SyntaxKind::SequenceKw,
        "shortint" => SyntaxKind::ShortintKw,
        "timeunit" => SyntaxKind::TimeunitKw,
        "unsigned" => SyntaxKind::UnsignedKw,
        "vectored" => SyntaxKind::VectoredKw,
        "wildcard" => SyntaxKind::WildcardKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_9(w: &str) -> SyntaxKind {
    match w {
        "accept_on" => SyntaxKind::AcceptOnKw,
        "always_ff" => SyntaxKind::AlwaysFfKw,
        "automatic" => SyntaxKind::AutomaticKw,
        "endconfig" => SyntaxKind::EndconfigKw,
        "endmodule" => SyntaxKind::EndmoduleKw,
        "interface" => SyntaxKind::InterfaceKw,
        "intersect" => SyntaxKind::IntersectKw,
        "join_none" => SyntaxKind::JoinNoneKw,
        "parameter" => SyntaxKind::ParameterKw,
        "primitive" => SyntaxKind::PrimitiveKw,
        "protected" => SyntaxKind::ProtectedKw,
        "reject_on" => SyntaxKind::RejectOnKw,
        "shortreal" => SyntaxKind::ShortRealKw,
        "specparam" => SyntaxKind::SpecparamKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_10(w: &str) -> SyntaxKind {
    match w {
        "constraint" => SyntaxKind::ConstraintKw,
        "covergroup" => SyntaxKind::CovergroupKw,
        "coverpoint" => SyntaxKind::CoverpointKw,
        "endchecker" => SyntaxKind::EndcheckerKw,
        "endpackage" => SyntaxKind::EndpackageKw,
        "endprogram" => SyntaxKind::EndprogramKw,
        "endspecify" => SyntaxKind::EndspecifyKw,
        "eventually" => SyntaxKind::EventuallyKw,
        "implements" => SyntaxKind::ImplementsKw,
        "localparam" => SyntaxKind::LocalparamKw,
        "s_nexttime" => SyntaxKind::SNexttimeKw,
        "throughout" => SyntaxKind::ThroughoutKw,
        "until_with" => SyntaxKind::UntilWithKw,
        "wait_order" => SyntaxKind::WaitOrderKw,
        _ => SyntaxKind::Ident,
    }
}

fn classify_long(w: &str) -> SyntaxKind {
    match w {
        "always_comb" => SyntaxKind::AlwaysCombKw,
        "always_latch" => SyntaxKind::AlwaysLatchKw,
        "endclocking" => SyntaxKind::EndclockingKw,
        "endfunction" => SyntaxKind::EndfunctionKw,
        "endgenerate" => SyntaxKind::EndgenerateKw,
        "endinterface" => SyntaxKind::EndinterfaceKw,
        "endprimitive" => SyntaxKind::EndprimitiveKw,
        "endproperty" => SyntaxKind::EndpropertyKw,
        "endsequence" => SyntaxKind::EndsequenceKw,
        "first_match" => SyntaxKind::FirstMatchKw,
        "ignore_bins" => SyntaxKind::IgnoreBinsKw,
        "illegal_bins" => SyntaxKind::IllegalBinsKw,
        "interconnect" => SyntaxKind::InterconnectKw,
        "macromodule" => SyntaxKind::MacromoduleKw,
        "noshowcancelled" => SyntaxKind::NoshowcancelledKw,
        "pulsestyle_ondetect" => SyntaxKind::PulsestyleOndetectKw,
        "pulsestyle_onevent" => SyntaxKind::PulsestyleOneventKw,
        "randsequence" => SyntaxKind::RandsequenceKw,
        "s_eventually" => SyntaxKind::SEventuallyKw,
        "s_until_with" => SyntaxKind::SUntilWithKw,
        "showcancelled" => SyntaxKind::ShowcancelledKw,
        "sync_accept_on" => SyntaxKind::SyncAcceptOnKw,
        "sync_reject_on" => SyntaxKind::SyncRejectOnKw,
        "timeprecision" => SyntaxKind::TimeprecisionKw,
        _ => SyntaxKind::Ident,
    }
}
