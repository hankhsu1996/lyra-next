use lyra_source::TextSize;

use crate::Site;

/// Active `default_nettype` value at a position (LRM 6.10 + 22.8).
///
/// Mirrors `lyra_preprocess::DefaultNettypeValue` for the semantic layer.
/// `lyra-db` constructs a `DefaultNettypePolicy` from preprocessor data
/// and passes it into resolve construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ActiveNetType {
    Wire,
    Tri,
    Tri0,
    Tri1,
    Wand,
    Triand,
    Wor,
    Trior,
    Trireg,
    Uwire,
    None,
}

impl ActiveNetType {
    /// Whether this value is `None` (implicit net creation disabled).
    pub fn is_none(self) -> bool {
        matches!(self, Self::None)
    }
}

/// Immutable policy view for implicit-net creation (LRM 6.10 + 22.8).
///
/// Constructed by `lyra-db` from the preprocessor's
/// `FileDefaultNettypeSummary`. Resolve queries this by site anchor
/// to decide diagnostic kind for unresolved names at implicit-net
/// candidate sites.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DefaultNettypePolicy {
    /// Sorted by offset. Each entry records an expanded offset where
    /// the policy changes and the new active net type.
    changes: Vec<(TextSize, ActiveNetType)>,
}

impl DefaultNettypePolicy {
    pub fn new(changes: Vec<(TextSize, ActiveNetType)>) -> Self {
        Self { changes }
    }

    /// Query the active net type at a given expanded offset.
    ///
    /// Returns `Wire` when no directive is active (LRM 22.8 default).
    pub fn active_at_offset(&self, offset: TextSize) -> ActiveNetType {
        let idx = self.changes.partition_point(|(o, _)| *o <= offset);
        if idx == 0 {
            ActiveNetType::Wire
        } else {
            self.changes[idx - 1].1
        }
    }

    /// Query the active net type at a use-site anchor.
    pub fn active_at_site(&self, site: Site) -> ActiveNetType {
        self.active_at_offset(site.text_range().start())
    }
}

#[cfg(test)]
mod tests {
    use lyra_source::TextSize;

    use super::{ActiveNetType, DefaultNettypePolicy};

    #[test]
    fn empty_policy_returns_wire() {
        let policy = DefaultNettypePolicy::default();
        assert_eq!(
            policy.active_at_offset(TextSize::new(100)),
            ActiveNetType::Wire
        );
    }

    #[test]
    fn single_change_before_offset() {
        let policy = DefaultNettypePolicy::new(vec![(TextSize::new(10), ActiveNetType::Tri)]);
        assert_eq!(
            policy.active_at_offset(TextSize::new(50)),
            ActiveNetType::Tri
        );
    }

    #[test]
    fn change_after_offset_ignored() {
        let policy = DefaultNettypePolicy::new(vec![(TextSize::new(100), ActiveNetType::Tri)]);
        assert_eq!(
            policy.active_at_offset(TextSize::new(50)),
            ActiveNetType::Wire
        );
    }

    #[test]
    fn multiple_changes_picks_last_applicable() {
        let policy = DefaultNettypePolicy::new(vec![
            (TextSize::new(10), ActiveNetType::Tri),
            (TextSize::new(30), ActiveNetType::None),
        ]);
        assert_eq!(
            policy.active_at_offset(TextSize::new(20)),
            ActiveNetType::Tri
        );
        assert_eq!(
            policy.active_at_offset(TextSize::new(50)),
            ActiveNetType::None
        );
    }

    #[test]
    fn none_detected() {
        let policy = DefaultNettypePolicy::new(vec![(TextSize::new(10), ActiveNetType::None)]);
        assert!(policy.active_at_offset(TextSize::new(50)).is_none());
    }

    #[test]
    fn at_exact_offset() {
        let policy = DefaultNettypePolicy::new(vec![(TextSize::new(10), ActiveNetType::Wand)]);
        assert_eq!(
            policy.active_at_offset(TextSize::new(10)),
            ActiveNetType::Wand
        );
    }
}
