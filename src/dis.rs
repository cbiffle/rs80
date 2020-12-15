use rs80_common::isa::{RegM, RegPair};
use rs80_common::insn_info::{Operand, FType, IType};
use std::io::{self, prelude::*};
use rs80_common::insn_info::InsnInfo;

static INSN_INFO: [InsnInfo; 256] = {
        use rs80_common::isa::*;
        use rs80_common::insn_info::*;

        include!(concat!(env!("OUT_DIR"), "/disassemble.rs"))
};

pub fn disassemble(bytes: &mut impl Iterator<Item = io::Result<u8>>,
                   out: &mut impl Write) -> io::Result<usize> {
    let table: &[_; 256] = &INSN_INFO;
    let opcode = match bytes.next() {
        None => return Ok(0),
        Some(op) => op?,
    };

    let mut used = vec![opcode];
    let mut buf = io::Cursor::new(vec![]);
    let insn = &table[opcode as usize];
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
        Operand::F(_, (f, v)) => match f {
            FType::RM => match RegM::from(v) {
                RegM::R(r) => write!(out, "{:?}", r)?,
                m => write!(out, "{:?}", m)?,
            },
            FType::RP => write!(out, "{:?}", RegPair::from(v))?,
            FType::C3 => write!(out, "{:02X}H", v * 8)?,
        },
        Operand::PSW => write!(out, "PSW")?,
        Operand::I(_, IType::I8) => if let Some(i) = f.next() {
            let i = i?;
            used.push(i);
            write!(out, "{:02X}H", i)?;
        } else {
            write!(out, "??")?;
        },
        Operand::I(_, IType::I16) | Operand::I(_, IType::Address) =>
            if let Some(lo) = f.next() {
                let lo = lo?;
                used.push(lo);
                if let Some(hi) = f.next() {
                    let hi = hi?;
                    used.push(hi);
                    write!(out, "{:04X}H", (hi as u16) << 8 | (lo as u16))?;
                } else {
                    write!(out, "??{:02X}H", lo)?;
                }
            } else {
                write!(out, "????H")?
            },
    }
    Ok(())
}
