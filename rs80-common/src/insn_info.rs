use super::isa::CC;

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

#[derive(Copy, Clone, Debug)]
pub struct Mnemonic {
    pub label: &'static str,
    pub cc: Option<CC>,
}

#[derive(Copy, Clone, Debug)]
pub struct InsnInfo {
    pub mnemonic: Mnemonic,
    pub a: Option<Operand>,
    pub b: Option<Operand>,
}
