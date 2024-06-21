//! ISA definition structures, parser, and codegen for rs80.

use rs80_common::insn_info::{FType, Operand};

pub mod parse;
pub mod gen;

/// An item found in the spec file.
#[derive(Clone, Debug)]
pub enum Item {
    /// An instruction definition.
    Def(Def),
    /// A comment. (We don't process comments, but this is here for
    /// consistency.)
    Comment(String),
}

/// An instruction definition.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Def {
    /// Bit pattern.
    pub bits: Pat,
    /// Mnemonic and optional condition code field identifier.
    pub mnem: (String, Option<char>),
    /// Operands from the assembly template.
    pub operands: Vec<AOperand>,
    /// Primary and optional secondary cycle count.
    pub cycles: (usize, Option<usize>),
    /// Rust code body.
    pub body: Vec<String>,
}

/// Comparison for `Def` is arranged so that definitions are sorted in order of
/// descending specificity -- that is, so that the earliest `Def` in a sorted
/// list that matches a given opcode byte is the correct decode.
impl Ord for Def {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Note: we can't _just_ compare on specificity because we need to be
        // consistent with Eq. So all fields must be used:
        self.bits.specificity().cmp(&other.bits.specificity()).reverse()
            .then_with(|| self.bits.cmp(&other.bits))
            .then_with(|| self.mnem.cmp(&other.mnem))
            .then_with(|| self.operands.cmp(&other.operands))
            .then_with(|| self.cycles.cmp(&other.cycles))
            .then_with(|| self.body.cmp(&other.body))
    }
}

impl PartialOrd for Def {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Customize the `Operand` type from common for the case where the operand
/// _value_ is not yet known.
pub type AOperand = Operand<FType>;

/// A bit pattern.
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Pat(Vec<PatPart>);

impl Pat {
    /// Checks whether a literal byte matches this pattern. If so, returns
    /// `Some(v)` where `v` maps field identifiers to values.
    pub fn matches(&self, mut val: u8) -> Option<Vec<(char, u8)>> {
        let mut fields = Vec::new();
        for part in &self.0 {
            if let Ok(r) = part.matches(val) {
                if let Some(field) = r {
                    fields.push(field)
                }
            } else {
                return None
            }
            if part.len() != 8 {
                val <<= part.len();
            } else {
                val = 0;
            }
        }
        Some(fields)
    }

    /// Counts literally specified bits in this pattern as a measure of pattern
    /// specificity. More specific patterns will be applied before less specific
    /// patterns.
    pub fn specificity(&self) -> usize {
        let mut s = 0;
        for part in &self.0 {
            if let PatPart::Bits(v) = part {
                s += v.len();
            }
        }
        s
    }
}

/// A field within a pattern.
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum PatPart {
    /// Some literal bits.
    Bits(Vec<bool>),
    /// A named field of the given number of bits.
    Var(char, usize),
    /// A certain number of don't care bits.
    Ignore(usize),
}

#[allow(clippy::len_without_is_empty, clippy::result_unit_err)]
impl PatPart {
    /// Determines the number of bits in this part.
    pub fn len(&self) -> usize {
        match self {
            PatPart::Bits(v) => v.len(),
            PatPart::Var(_, n) => *n,
            PatPart::Ignore(n) => *n,
        }
    }

    /// Checks whether this part matches the *most significant bits* of `val`.
    ///
    /// If literal bits fail to match, returns `Err`.
    ///
    /// Otherwise, returns `Ok(Some((n, v)))` if a field `v` collected bits `n`,
    /// or `Ok(None)` if the bits were literal match or don't-care.
    pub fn matches(&self, mut val: u8) -> Result<Option<(char, u8)>, ()> {
        match self {
            PatPart::Bits(v) => {
                for bit in v {
                    if *bit != ((val & 0x80) != 0) {
                        return Err(())
                    }
                    val <<= 1;
                }
                Ok(None)
            },
            PatPart::Var(c, n) => {
                Ok(Some((*c, val >> (8 - n))))
            },
            PatPart::Ignore(_) => Ok(None),
        }
    }
}
