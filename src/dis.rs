use super::isa::{CC, RegM, RegPair};

#[derive(Copy, Clone, Debug)]
pub enum Operand {
    RM(RegM),
    RP(RegPair),
    PSW,
    C3(u8),
    CC(CC),
    I8,
    Port8,
    I16,
    Addr,
}

impl From<RegM> for Operand {
    fn from(v: RegM) -> Operand { Operand::RM(v) }
}

impl From<RegPair> for Operand {
    fn from(v: RegPair) -> Operand { Operand::RP(v) }
}

impl From<u8> for Operand {
    fn from(v: u8) -> Operand { Operand::C3(v) }
}

impl From<CC> for Operand {
    fn from(v: CC) -> Operand { Operand::CC(v) }
}

#[derive(Copy, Clone, Debug)]
pub struct Mnemonic {
    pub label: &'static str,
    pub cc: Option<CC>,
}

impl From<&'static str> for Mnemonic {
    fn from(v: &'static str) -> Mnemonic {
        Mnemonic { label: v, cc: None }
    }
}

impl From<(&'static str, CC)> for Mnemonic {
    fn from(v: (&'static str, CC)) -> Mnemonic {
        Mnemonic { label: v.0, cc: Some(v.1) }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct InsnInfo {
    pub mnemonic: Mnemonic,
    pub a: Option<Operand>,
    pub b: Option<Operand>,
}

impl InsnInfo {
    pub fn inherent(m: impl Into<Mnemonic>) -> Self {
        InsnInfo { mnemonic: m.into(), a: None, b: None }
    }

    pub fn binary(m: impl Into<Mnemonic>,
                  d: impl Into<Operand>, s: impl Into<Operand>) -> Self
    {
        InsnInfo { mnemonic: m.into(), a: Some(d.into()), b: Some(s.into()) }
    }

    pub fn unary(m: impl Into<Mnemonic>, a: impl Into<Operand>) -> Self
    {
        InsnInfo { mnemonic: m.into(), a: Some(a.into()), b: None }

    }
}
