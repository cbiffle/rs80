use super::isa::{CC, RegM, RegPair};

#[derive(Copy, Clone, Debug)]
pub enum Operand<FT = (FType, u8)> {
    F(char, FT),
    I(char, IType),
    PSW,
}

#[derive(Copy, Clone, Debug)]
pub enum FType {
    RM,
    RP,
    C3,
}

#[derive(Copy, Clone, Debug)]
pub enum IType {
    I8,
    I16,
    Address,
}

impl Operand {
    fn addl_bytes(self) -> usize {
        match self {
            Operand::I(_, IType::I8) => 1,
            Operand::I(_, IType::I16) => 2,
            _ => 0,
        }
    }
}

impl From<RegM> for Operand {
    fn from(v: RegM) -> Operand { Operand::F('?', (FType::RM, v.into())) }
}

impl From<RegPair> for Operand {
    fn from(v: RegPair) -> Operand { Operand::F('?', (FType::RP, v as u8)) }
}

impl From<u8> for Operand {
    fn from(v: u8) -> Operand { Operand::F('?', (FType::C3, v as u8)) }
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
    pub fn len(&self) -> usize {
        1 + self.a.map(|o| o.addl_bytes()).unwrap_or(0)
          + self.b.map(|o| o.addl_bytes()).unwrap_or(0)
    }
}
