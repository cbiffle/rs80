use super::isa::{CC, RegM, RegPair, Opcode};
use super::ops;

use std::io::{self, prelude::*};

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

    pub fn len(&self) -> usize {
        1 + self.a.map(|o| o.addl_bytes()).unwrap_or(0)
          + self.b.map(|o| o.addl_bytes()).unwrap_or(0)
    }
}

pub fn disassemble(bytes: &mut impl Iterator<Item = io::Result<u8>>,
                   out: &mut impl Write) -> io::Result<usize> {
    let table = ops::make_decode_table();
    let opcode = match bytes.next() {
        None => return Ok(0),
        Some(op) => op?,
    };

    let mut n = 1;
    match table[opcode as usize] {
        None => write!(out, "DB {:02X}", opcode)?,
        Some(f) => {
            let insn = f(Opcode(opcode));
            if let Some(cc) = insn.mnemonic.cc {
                write!(out, "{}{:?}", insn.mnemonic.label, cc)?;
            } else {
                write!(out, "{}", insn.mnemonic.label)?;
            }
            if let Some(a) = insn.a {
                write!(out, "\t")?;
                n += print_operand(a, bytes, out)?;
                if let Some(b) = insn.b {
                    write!(out, ",")?;
                    n += print_operand(b, bytes, out)?;
                }
            }
        },
    }

    Ok(n)
}

fn print_operand(op: Operand,
                 f: &mut impl Iterator<Item=io::Result<u8>>,
                 out: &mut impl Write) -> io::Result<usize>
{
    match op {
        Operand::RM(RegM::R(r)) => write!(out, "{:?}", r)?,
        Operand::RM(m) => write!(out, "{:?}", m)?,
        Operand::RP(rp) => write!(out, "{:?}", rp)?,
        Operand::PSW => write!(out, "PSW")?,
        Operand::C3(c) => write!(out, "{:02X}H", c * 8)?,
        Operand::I8 | Operand::Port8 => if let Some(i) = f.next() {
            write!(out, "{:02X}H", i?)?;
            return Ok(1)
        } else {
            write!(out, "???")?;
            return Ok(0)
        },
        Operand::I16 | Operand::Addr => if let Some(lo) = f.next() {
            if let Some(hi) = f.next() {
                write!(out, "{:04X}H", (hi? as u16) << 8 | (lo? as u16))?;
                return Ok(2)
            } else {
                write!(out, "???")?;
                return Ok(1)
            }
        } else {
            write!(out, "???")?
        },
    }
    return Ok(0)
}
