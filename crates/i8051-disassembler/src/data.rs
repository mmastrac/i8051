use crate::address::AddressValue;

pub struct DataHeuristics {
    /// Bytes per `.db` row; also the unit for alignment and row-repeat.
    pub block_size: usize,

    /// How literal rows break for display; does not affect byte layout.
    pub row_alignment: RowAlignment,

    /// Collapse N+ identical block_size rows into one `.rept` (hexdump `*`).
    /// None disables multi-byte row folding.
    pub min_repeat_rows: Option<usize>,

    /// RLE rules applied to runs in the body of the span.
    pub interior: Vec<RunRule>,

    /// Trim at the start of a mapped chunk (None = leave leading bytes literal).
    pub leading: Option<EdgeTrim>,

    /// Trim at the end of a mapped chunk (None = leave trailing bytes literal).
    pub trailing: Option<EdgeTrim>,

    /// When the entire span is zeros and length is at least this value, emit one
    /// skip run for the whole span. `usize::MAX` disables the fast path.
    pub all_zero_single_ds_min: usize,

    /// When set, run compression only applies to the address-aligned middle of a
    /// homogeneous run spanning at least this many `block_size` blocks.
    pub min_aligned_blocks: Option<usize>,

    /// Emit zero skip runs as multiple `.ds block_size` lines instead of one
    /// `.ds N` for the full run length.
    pub zero_skip_windowed: bool,
}

pub enum RowAlignment {
    /// Pack `block_size` bytes from the start of each literal span.
    Packed,
    /// Align rows to the mapped chunk start.
    Block,
    /// Align rows to absolute address (xxd-style).
    Global,
}

#[derive(Clone)]
pub struct RunRule {
    /// `Some(v)` matches a run of value `v`; `None` is the catch-all
    /// (any repeated byte), evaluated only after all `Some` rules.
    pub value: Option<u8>,
    /// Minimum run length before this rule fires; shorter runs stay inline.
    /// `None` means always literal.
    pub min_run: Option<usize>,
}

#[derive(Clone)]
pub struct EdgeTrim {
    /// Run rules for this edge — may use different values / lower thresholds
    /// than `interior` (edge padding is unambiguous, so trim sooner).
    pub rules: Vec<RunRule>,
    /// Literal bytes kept flush against the boundary, outside the fill run.
    /// This is the DOS-ROM tail: e.g. 4 for a u32 checksum, 2 for a u16 sig.
    pub reserve: usize,
}

pub enum Edge {
    Leading,
    Trailing,
}

impl Default for DataHeuristics {
    fn default() -> Self {
        Self {
            block_size: 16,
            row_alignment: RowAlignment::Block,
            min_repeat_rows: None,
            interior: vec![
                RunRule {
                    value: Some(0x00),
                    min_run: Some(8),
                },
                RunRule {
                    value: Some(0xFF),
                    min_run: Some(64),
                },
            ],
            leading: None,
            trailing: Some(EdgeTrim {
                rules: vec![
                    RunRule {
                        value: Some(0xFF),
                        min_run: Some(16),
                    },
                    RunRule {
                        value: Some(0x00),
                        min_run: Some(4),
                    },
                ],
                reserve: 4,
            }),
            all_zero_single_ds_min: 8,
            min_aligned_blocks: None,
            zero_skip_windowed: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RuleDecision {
    Compress,
    ForceLiteral,
    NoMatch,
}

impl DataHeuristics {
    pub fn iterate<'a>(
        &self,
        address: AddressValue,
        edge: Option<Edge>,
        bytes: &'a [u8],
    ) -> impl Iterator<Item = DataChunk<'a, u8>> {
        self.segment(address, edge, bytes).into_iter()
    }

    /// Split a literal span into `.db` rows per `block_size` and `row_alignment`.
    pub fn literal_rows<'a>(&self, address: AddressValue, bytes: &'a [u8]) -> Vec<&'a [u8]> {
        if bytes.is_empty() {
            return Vec::new();
        }
        let bs = self.block_size.max(1);
        match self.row_alignment {
            RowAlignment::Packed | RowAlignment::Block => bytes.chunks(bs).map(|row| row).collect(),
            RowAlignment::Global => {
                let mut rows = Vec::new();
                let mut i = 0;
                let start_mod = (address as usize) % bs;
                if start_mod != 0 {
                    let first_len = (bs - start_mod).min(bytes.len());
                    rows.push(&bytes[0..first_len]);
                    i = first_len;
                }
                while i < bytes.len() {
                    let end = (i + bs).min(bytes.len());
                    rows.push(&bytes[i..end]);
                    i = end;
                }
                rows
            }
        }
    }

    fn segment<'a>(
        &self,
        _address: AddressValue,
        _edge: Option<Edge>,
        bytes: &'a [u8],
    ) -> Vec<DataChunk<'a, u8>> {
        let mut out = Vec::new();
        let len = bytes.len();
        if len == 0 {
            return out;
        }

        // reserve carves literal head/tail; runs are only detected in the window
        let head = self.leading.as_ref().map_or(0, |e| e.reserve).min(len);
        let tail = self
            .trailing
            .as_ref()
            .map_or(0, |e| e.reserve)
            .min(len - head);
        let win_start = head;
        let win_end = len - tail;

        let mut lit_start = 0usize; // pending literal span start (absolute, incl. reserved head)
        let mut i = win_start;

        while i < win_end {
            let value = bytes[i];
            let mut j = i + 1;
            while j < win_end && bytes[j] == value {
                j += 1;
            }
            let run_len = j - i;
            let at_start = i == win_start;
            let at_end = j == win_end;

            if self.should_compress(value, run_len, at_start, at_end) {
                push_literal(&mut out, bytes, lit_start, i);
                out.push(DataChunk::Run(value, run_len));
                i = j;
                lit_start = j;
                continue;
            }

            if let Some(count) = self.block_run_at(bytes, i, win_end) {
                let unit_len = self.block_size;
                push_literal(&mut out, bytes, lit_start, i);
                out.push(DataChunk::BlockRun(&bytes[i..i + unit_len], count));
                i += unit_len * count;
                lit_start = i;
                continue;
            }

            // equal-run wasn't compressible: leave it in the pending literal span
            i = j;
        }

        // flush trailing in-window literal + reserved tail as one span
        push_literal(&mut out, bytes, lit_start, len);
        out
    }

    fn should_compress(&self, value: u8, run_len: usize, at_start: bool, at_end: bool) -> bool {
        let decide = |rules: &[RunRule]| rule_decision(rules, value, run_len);

        // edge rule sets (relaxed thresholds) take precedence over interior;
        // a matching `Literal` rule short-circuits to "don't compress"
        if at_start {
            if let Some(edge) = &self.leading {
                match decide(&edge.rules) {
                    RuleDecision::Compress => return true,
                    RuleDecision::ForceLiteral => return false,
                    RuleDecision::NoMatch => {}
                }
            }
        }
        if at_end {
            if let Some(edge) = &self.trailing {
                match decide(&edge.rules) {
                    RuleDecision::Compress => return true,
                    RuleDecision::ForceLiteral => return false,
                    RuleDecision::NoMatch => {}
                }
            }
        }
        matches!(decide(&self.interior), RuleDecision::Compress)
    }

    fn block_run_at(&self, bytes: &[u8], i: usize, end: usize) -> Option<usize> {
        let min_rows = self.min_repeat_rows?;
        let bs = self.block_size;
        if bs == 0 || min_rows == 0 || i + bs.checked_mul(min_rows)? > end {
            return None;
        }
        let unit = &bytes[i..i + bs];
        let mut count = 1;
        let mut pos = i + bs;
        while pos + bs <= end && &bytes[pos..pos + bs] == unit {
            count += 1;
            pos += bs;
        }
        (count >= min_rows).then_some(count)
    }
}

fn rule_decision(rules: &[RunRule], value: u8, run_len: usize) -> RuleDecision {
    // exact-value rules before the `None` catch-all
    for rule in rules.iter().filter(|r| r.value == Some(value)) {
        if let Some(min_run) = rule.min_run {
            if run_len >= min_run {
                return RuleDecision::Compress;
            }
        } else {
            return RuleDecision::ForceLiteral;
        }
    }
    for rule in rules.iter().filter(|r| r.value.is_none()) {
        if let Some(min_run) = rule.min_run {
            if run_len >= min_run {
                return RuleDecision::Compress;
            }
        } else {
            return RuleDecision::ForceLiteral;
        }
    }
    RuleDecision::NoMatch
}

fn push_literal<'a>(out: &mut Vec<DataChunk<'a, u8>>, bytes: &'a [u8], start: usize, end: usize) {
    if start < end {
        out.push(DataChunk::Literal(&bytes[start..end]));
    }
}

/// A processed chunk of data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataChunk<'a, T> {
    /// A row sequence of literals.
    Literal(&'a [T]),
    /// A run of a single value.
    Run(T, usize),
    /// A run of a single value that is a multiple of the block size.
    BlockRun(&'a [T], usize),
}

#[cfg(test)]
mod tests {
    use super::*;
    use DataChunk::*;

    fn rule(value: Option<u8>, min_run: usize) -> RunRule {
        RunRule {
            value,
            min_run: Some(min_run),
        }
    }

    fn rule_literal(value: Option<u8>) -> RunRule {
        RunRule {
            value,
            min_run: None,
        }
    }

    fn heur(
        interior: Vec<RunRule>,
        leading: Option<EdgeTrim>,
        trailing: Option<EdgeTrim>,
    ) -> DataHeuristics {
        DataHeuristics {
            block_size: 4,
            interior,
            leading,
            trailing,
            ..Default::default()
        }
    }

    fn run<'a>(h: &DataHeuristics, b: &'a [u8]) -> Vec<DataChunk<'a, u8>> {
        h.iterate(0, None, b).collect()
    }

    #[test]
    fn empty_yields_nothing() {
        assert!(run(&heur(vec![], None, None), &[]).is_empty());
    }

    #[test]
    fn all_literal_is_one_maximal_span() {
        let h = heur(vec![rule(Some(0x00), 4)], None, None);
        assert_eq!(run(&h, &[1, 2, 3, 4, 5]), vec![Literal(&[1, 2, 3, 4, 5])]);
    }

    #[test]
    fn interior_run_compresses_and_splits_literals() {
        let h = heur(vec![rule(Some(0x00), 4)], None, None);
        assert_eq!(
            run(&h, &[1, 2, 0, 0, 0, 0, 0, 3]),
            vec![Literal(&[1, 2]), Run(0x00, 5), Literal(&[3])]
        );
    }

    #[test]
    fn run_below_min_stays_literal() {
        let h = heur(vec![rule(Some(0x00), 4)], None, None);
        assert_eq!(run(&h, &[1, 0, 0, 2]), vec![Literal(&[1, 0, 0, 2])]);
    }

    #[test]
    fn min_run_boundary_for_repeat() {
        let h = heur(vec![rule(Some(0xFF), 8)], None, None);
        // 7 bytes: below min, whole thing literal
        assert_eq!(
            run(&h, &[1, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 2]),
            vec![Literal(&[1, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 2])]
        );
        // 8 bytes: compresses
        assert_eq!(
            run(&h, &[1, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 2]),
            vec![Literal(&[1]), Run(0xFF, 8), Literal(&[2])]
        );
    }

    #[test]
    fn catch_all_matches_any_value() {
        let h = heur(vec![rule(None, 4)], None, None);
        assert_eq!(run(&h, &[0x42, 0x42, 0x42, 0x42, 0x42]), vec![Run(0x42, 5)]);
    }

    #[test]
    fn literal_rule_short_circuits_catch_all() {
        let h = heur(vec![rule_literal(Some(0x20)), rule(None, 1)], None, None);
        assert_eq!(run(&h, &[0x20; 6]), vec![Literal(&[0x20; 6])]);
    }

    #[test]
    fn trailing_reserve_preserves_checksum() {
        let trailing = EdgeTrim {
            rules: vec![rule(Some(0xFF), 4)],
            reserve: 2,
        };
        let h = heur(vec![], None, Some(trailing));
        let bytes = [10, 11, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xAA, 0xBB];
        assert_eq!(
            run(&h, &bytes),
            vec![Literal(&[10, 11]), Run(0xFF, 6), Literal(&[0xAA, 0xBB])]
        );
    }

    #[test]
    fn leading_reserve_preserves_signature() {
        let leading = EdgeTrim {
            rules: vec![rule(Some(0x00), 2)],
            reserve: 2,
        };
        let h = heur(vec![], Some(leading), None);
        let bytes = [0xAA, 0x55, 0, 0, 0, 0, 10];
        assert_eq!(
            run(&h, &bytes),
            vec![Literal(&[0xAA, 0x55]), Run(0x00, 4), Literal(&[10])]
        );
    }

    #[test]
    fn edge_rule_relaxes_interior_threshold() {
        let interior = vec![rule(Some(0xFF), 64)];
        let trailing = EdgeTrim {
            rules: vec![rule(Some(0xFF), 4)],
            reserve: 0,
        };
        let h = heur(interior, None, Some(trailing));
        // interior min 64 would not fire, trailing min 4 does
        assert_eq!(
            run(&h, &[1, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
            vec![Literal(&[1]), Run(0xFF, 5)]
        );
    }

    #[test]
    fn literal_rows_pack_by_block_size() {
        let h = DataHeuristics::default();
        let bytes = (0..40).collect::<Vec<_>>();
        let rows = h.literal_rows(0, &bytes);
        assert_eq!(rows.len(), 3);
        assert_eq!(rows[0].len(), 16);
        assert_eq!(rows[1].len(), 16);
        assert_eq!(rows[2].len(), 8);
    }

    #[test]
    fn literal_rows_global_aligns_to_address() {
        let mut h = DataHeuristics::default();
        h.row_alignment = RowAlignment::Global;
        let bytes = (0..20).collect::<Vec<_>>();
        let rows = h.literal_rows(4, &bytes);
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].len(), 12);
        assert_eq!(rows[1].len(), 8);
    }

    #[test]
    fn block_run_folds_repeated_unit() {
        let mut h = heur(vec![], None, None);
        h.block_size = 2;
        h.min_repeat_rows = Some(3);
        assert_eq!(
            run(&h, &[0xDE, 0xAD, 0xDE, 0xAD, 0xDE, 0xAD]),
            vec![BlockRun(&[0xDE, 0xAD], 3)]
        );
    }

    #[test]
    fn single_value_run_wins_over_block_run() {
        let mut h = heur(vec![rule(Some(0xFF), 4)], None, None);
        h.block_size = 2;
        h.min_repeat_rows = Some(2);
        assert_eq!(run(&h, &[0xFF; 6]), vec![Run(0xFF, 6)]);
    }
}
