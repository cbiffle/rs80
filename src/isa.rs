/// Names and encodings of 8-bit registers used in operand positions.
#[derive(Copy, Clone, Debug)]
pub enum Reg {
    B = 0b000,
    C = 0b001,
    D = 0b010,
    E = 0b011,
    H = 0b100,
    L = 0b101,
    // M = 0b110 but is handled separately (see RegM)
    A = 0b111,
}

/// Parsing three-bit register operand field.
impl From<u8> for Reg {
    fn from(x: u8) -> Reg {
        match x {
            0b000 => Reg::B,
            0b001 => Reg::C,
            0b010 => Reg::D,
            0b011 => Reg::E,
            0b100 => Reg::H,
            0b101 => Reg::L,
            0b110 => panic!("M encoding for Reg made it past decoder"),
            0b111 => Reg::A,
            _ => panic!("Encoding for Reg out of range: {}", x),
        }
    }
}

/// Operands that can accomodate either a register or memory without
/// significantly changing the meaning of the instruction.
///
/// We use this for e.g. arithmetic operations that act on the accumulator and
/// *either* a register or memory location (addressed through HL).
///
/// Operations that take either a register or an *immediate* are encoded
/// similarly, but are semantically very different, and don't use this type.
#[derive(Copy, Clone, Debug)]
pub enum RegM {
    /// Memory operand.
    M,
    /// Register operand.
    R(Reg),
}

impl RegM {
    /// Returns one value or the other, depending on whether `self` is a
    /// register or memory operand. This is useful for changing cycle counts.
    pub fn reg_or_m<T>(self, r: T, m: T) -> T {
        match self {
            RegM::M => m,
            RegM::R(_) => r,
        }
    }
}

/// Parsing three-bit operand field.
impl From<u8> for RegM {
    fn from(x: u8) -> RegM {
        match x {
            0b110 => RegM::M,
            _ => RegM::R(x.into()),
        }
    }
}

/// A 16-bit register pair operand. Note that the bits that would decode as `SP`
/// sometimes pun for a completely different instruction.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RegPair {
    BC = 0b00,
    DE = 0b01,
    HL = 0b10,
    SP = 0b11,
}

impl From<u8> for RegPair {
    fn from(x: u8) -> RegPair {
        match x {
            0b00 => RegPair::BC,
            0b01 => RegPair::DE,
            0b10 => RegPair::HL,
            0b11 => RegPair::SP,
            _ => panic!("Encoding for RegPair out of range: {}", x),
        }
    }
}

/// Condition codes used in conditional instructions.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CC {
    NZ = 0b000,
    Z  = 0b001,
    NC = 0b010,
    C  = 0b011,
    PO = 0b100,
    PE = 0b101,
    P  = 0b110,
    N  = 0b111,
}

/// Parsing three-bit condition field into `CC`.
impl From<u8> for CC {
    fn from(x: u8) -> CC {
        match x {
            0b000 => CC::NZ,
            0b001 => CC::Z ,
            0b010 => CC::NC,
            0b011 => CC::C ,
            0b100 => CC::PO,
            0b101 => CC::PE,
            0b110 => CC::P ,
            0b111 => CC::N ,
            _ => panic!("Encoding for CC out of range: {}", x),
        }
    }
}

/// A byte-sized instruction opcode.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Opcode(pub u8);

impl Opcode {
    /// Extracts bits `[hi:lo]` (inclusive) as a `T`.
    pub fn bits<T>(self, hi: u32, lo: u32) -> T
        where u8: Into<T>
    {
        debug_assert!(hi >= lo,
                      "backwards bit range {}:{}", hi, lo);
        debug_assert!(hi < 8 && lo < 8,
                      "bits out of range for u8: {}:{}", hi, lo);
        let width = hi - lo + 1;
        ((self.0 >> lo) & ((1 << width) - 1)).into()
    }
}


/// Alias for `u8` when serving as an immediate.
pub type Imm8 = u8;
/// Alias for `u16` when serving as an immediate.
pub type Imm16 = u16;
/// Alias for `u16` when serving as a direct memory address.
pub type Addr = u16;
/// Alias for `u8` when serving as a restart address.
pub type Restart = u8;
/// Alias for `u8` when serving as a literal port number.
pub type Port = u8;

/// High-level encoding of the 8080 instruction set.
///
/// This isn't used in the emulator, but is used by debugging support.
///
/// This encoding is both over-general and lossy: over-general in that it can
/// encode instructions that are not actually legal (see the `is_valid`
/// predicate), and lossy in that instructions with multiple representations all
/// decode to the same `Insn`.
#[derive(Copy, Clone, Debug)]
pub enum Insn {
    Mov(Reg, Reg),
    MovToM(Reg),
    MovFromM(Reg),

    Mvi(Reg, Imm8),
    MviToM(Imm8),

    Lxi(RegPair, Imm16),

    Lda(Addr),
    Sta(Addr),
    Lhld(Addr),
    Shld(Addr),
    Ldax(RegPair),
    Stax(RegPair),

    Xchg,

    Add(RegM),
    Adi(Imm8),
    Adc(RegM),
    Aci(Imm8),
    Sub(RegM),
    Sui(Imm8),
    Sbb(RegM),
    Sbi(Imm8),
    Inr(RegM),
    Dcr(RegM),
    Inx(RegPair),
    Dcx(RegPair),
    Dad(RegPair),
    
    Daa,

    Ana(RegM),
    Ani(Imm8),
    Ora(RegM),
    Ori(Imm8),
    Xra(RegM),
    Xri(Imm8),
    Cmp(RegM),
    Cpi(Imm8),
    
    Rlc,
    Rrc,
    Ral,
    Rar,
    Cma,
    Cmc,
    Stc,

    Jmp(Addr),
    J(CC, Addr),
    Call(Addr),
    C(CC, Addr),
    Ret,
    R(CC),
    Rst(Restart),

    Pchl,
    Push(RegPair),
    PushPSW,
    Pop(RegPair),
    PopPSW,
    Xthl,
    Sphl,
    In(Port),
    Out(Port),
    Ei,
    Di,
    Hlt,
    Nop,
}
