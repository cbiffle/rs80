use super::isa::{CC, RegM, RegPair};

#[derive(Copy, Clone, Debug)]
pub enum Operand {
    RM(RegM),
    RP(RegPair),
    PSW,
    C3(u8),
    I8,
    Port8,
    I16,
    Addr,
}

impl Operand {
    fn addl_bytes(self) -> usize {
        match self {
            Operand::I8 | Operand::Port8 => 1,
            Operand::I16 | Operand::Addr => 2,
            _ => 0,
        }
    }
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
