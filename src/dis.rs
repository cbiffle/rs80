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

    let mut used = vec![opcode];
    let mut buf = io::Cursor::new(vec![]);
    match table[opcode as usize] {
        None => write!(&mut buf, "DB\t{:02X}", opcode)?,
        Some(f) => {
            let insn = f(Opcode(opcode));
            if let Some(cc) = insn.mnemonic.cc {
                write!(&mut buf, "{}{:?}", insn.mnemonic.label, cc)?;
            } else {
                write!(&mut buf, "{}", insn.mnemonic.label)?;
            }
            if let Some(a) = insn.a {
                write!(&mut buf, "\t")?;
                print_operand(a, bytes, &mut used, &mut buf)?;
                if let Some(b) = insn.b {
                    write!(&mut buf, ",")?;
                    print_operand(b, bytes, &mut used, &mut buf)?;
                }
            }
        },
    }

    let text = String::from_utf8(buf.into_inner()).unwrap();
    for b in &used {
        write!(out, "{:02X} ", b)?;
    }
    for _ in used.len() .. 3 {
        write!(out, "   ")?;
    }
    write!(out, "\t{}", text)?;

    Ok(used.len())
}

fn print_operand(op: Operand,
                 f: &mut impl Iterator<Item=io::Result<u8>>,
                 used: &mut Vec<u8>,
                 out: &mut impl Write) -> io::Result<()>
{
    match op {
        Operand::RM(RegM::R(r)) => write!(out, "{:?}", r)?,
        Operand::RM(m) => write!(out, "{:?}", m)?,
        Operand::RP(rp) => write!(out, "{:?}", rp)?,
        Operand::PSW => write!(out, "PSW")?,
        Operand::C3(c) => write!(out, "{:02X}H", c * 8)?,
        Operand::I8 | Operand::Port8 => if let Some(i) = f.next() {
            let i = i?;
            used.push(i);
            write!(out, "{:02X}H", i)?;
        } else {
            write!(out, "???")?;
        },
        Operand::I16 | Operand::Addr => if let Some(lo) = f.next() {
            let lo = lo?;
            used.push(lo);
            if let Some(hi) = f.next() {
                let hi = hi?;
                used.push(hi);
                write!(out, "{:04X}H", (hi as u16) << 8 | (lo as u16))?;
            } else {
                write!(out, "???")?;
            }
        } else {
            write!(out, "???")?
        },
    }
    Ok(())
}
