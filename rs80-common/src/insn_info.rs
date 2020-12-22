//! Types used to specify the human-readable representation of 8080
//! instructions.

use super::isa::CC;

/// A description of an explicit operand, for instructions that accept them.
/// (Some instructions, like `CMA`, use implicit operands only.)
///
/// `FT` is a parameter because it's used in two places with slightly different
/// requirements:
///
/// 1. When describing a class of instructions, `FT` is `FType`.
/// 2. When describing a *particular* instruction, `FT` is `(FType, u8)`, giving
///    both the type and the *actual value* of the decoded instruction.
#[derive(Copy, Clone, Debug)]
pub enum Operand<FT = (FType, u8)> {
    /// A "field" operand encoded into the opcode byte.
    F(char, FT),
    /// An "immediate" operand that follows the opcode byte.
    I(char, IType),
    /// The special operand used to signal push/pop of the AF register pair.
    /// This is represented as an explicit operand in assembly source, but we
    /// treat it as a special field operand under the hood.
    PSW,
}

impl<FT> Operand<FT> {
    /// Calculate the number of bytes (in addition to the opcode) required to
    /// encode this operand.
    pub fn length(&self) -> usize {
        if let Self::I(_, t) = self {
            t.length()
        } else {
            0
        }
    }
}

/// Types of field operands.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FType {
    /// 8-bit register or memory location. 3 bits wide. Memory, if used, is
    /// addressed indirectly through HL. This corresponds to the `RegM` type
    /// from `isa`.
    RM,
    /// 16-bit register pair. 2 bits wide.
    RP,
    /// 3-bit constant embedded in the opcode. This is only used for `RST`.
    C3,
}

/// Types of immediate operands.
#[derive(Copy, Clone, Debug)]
pub enum IType {
    /// 8-bit immediate byte.
    I8,
    /// 16-bit immediate word, little-endian order.
    I16,
    /// 16-bit immediate address, little-endian order. This is distinguished
    /// from `I16` in case it proves useful to the disassembler; so far, it has
    /// not.
    Address,
}

impl IType {
    /// Calculate the number of bytes (in addition to the opcode) required to
    /// encode this immediate.
    pub fn length(&self) -> usize {
        match self {
            Self::I8 => 1,
            Self::I16 | Self::Address => 2,
        }
    }
}

/// Textual mnemonic for an instruction, with optional condition code for
/// jump/call/return. If present, the condition code is concatenated to the base
/// label.
#[derive(Copy, Clone, Debug)]
pub struct Mnemonic {
    /// Base label for the instruction, e.g. `STAX` or `J`.
    pub label: &'static str,
    /// Condition code for conditional control flow.
    pub cc: Option<CC>,
}

/// Description of an instruction.
///
/// This type uses a pair of `Option` to represent a collection of 0, 1, or 2
/// operands. This is arguably a hack.
#[derive(Copy, Clone, Debug)]
pub struct InsnInfo {
    pub mnemonic: Mnemonic,
    /// First explicit operand, if any.
    pub a: Option<Operand>,
    /// Second explicit operand, if present.
    pub b: Option<Operand>,
}

impl InsnInfo {
    /// Calculate the number of bytes required to encode this instruction.
    pub fn length(&self) -> usize {
        1 + self.a.map(|o| o.length()).unwrap_or(0)
          + self.b.map(|o| o.length()).unwrap_or(0)
    }
}
