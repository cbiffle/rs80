//! ISA definition structures, parser, and codegen for rs80.

extern crate combine;
extern crate rs80_common;

use rs80_common::insn_info::{FType, Operand};

pub mod parse;
pub mod gen;

/// An item found in the spec file.
#[derive(Clone, Debug)]
pub enum Item {
    Def(Def),
    Comment(String),
}

/// An instruction definition.
#[derive(Clone, Debug)]
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

pub type AOperand = Operand<FType>;

/// A bit pattern.
#[derive(Clone, Debug)]
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
            if let &PatPart::Bits(ref v) = part {
                s += v.len();
            }
        }
        s
    }
}

/// A field within a pattern.
#[derive(Clone, Debug)]
pub enum PatPart {
    /// Some literal bits.
    Bits(Vec<bool>),
    /// A named field of the given number of bits.
    Var(char, usize),
    /// A certain number of don't care bits.
    Ignore(usize),
}

impl PatPart {
    /// Determines the number of bits in this part.
    pub fn len(&self) -> usize {
        match self {
            &PatPart::Bits(ref v) => v.len(),
            &PatPart::Var(_, n) => n,
            &PatPart::Ignore(n) => n,
        }
    }

    /// Checks whether this part matches the *most significant bits* of `val`.
    ///
    /// If literal bits fail to match, returns `Err`.
    ///
    /// Otherwise, returns `Ok(Some((n, v)))` if a field `v` collected bits `n`,
    /// or `Ok(None)` if the bits were don't-care.
    pub fn matches(&self, mut val: u8) -> Result<Option<(char, u8)>, ()> {
        match self {
            &PatPart::Bits(ref v) => {
                for bit in v {
                    if *bit != ((val & 0x80) != 0) {
                        return Err(())
                    }
                    val = val << 1;
                }
                Ok(None)
            },
            &PatPart::Var(c, n) => {
                Ok(Some((c, val >> (8 - n))))
            },
            &PatPart::Ignore(_) => Ok(None),
        }
    }
}
