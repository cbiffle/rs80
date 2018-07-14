extern crate rs80;

use std::io::{self, Read};
use std::env;
use std::fs;

use rs80::dis::Operand;
use rs80::isa::RegM;

fn main() -> io::Result<()> {
    let mut args = env::args();
    args.next();
    let filename = match args.next() {
        None => panic!("filename argument missing"),
        Some(f) => f,
    };

    let table = rs80::make_decode_table();

    let mut file = fs::File::open(filename)?.bytes().enumerate()
        .map(|(i, r)| (i + 0x100, r));
    while let Some((addr, opcode)) = file.next() {
        let opcode = opcode?;

        print!("{:04X}\t", addr);
        match table[opcode as usize] {
            None => println!("DB {:02X}", opcode),
            Some(f) => {
                let insn = f(rs80::isa::Opcode(opcode));
                if let Some(cc) = insn.mnemonic.cc {
                    print!("{}{:?}", insn.mnemonic.label, cc);
                } else {
                    print!("{}", insn.mnemonic.label);
                }
                if let Some(a) = insn.a {
                    print!("\t");
                    print_operand(a, &mut file)?;
                    if let Some(b) = insn.b {
                        print!(",");
                        print_operand(b, &mut file)?;
                    }
                }
                print!("\n");
            },
        }
    }
    Ok(())
}

fn print_operand(op: Operand,
                 f: &mut impl Iterator<Item=(usize, io::Result<u8>)>)
    -> io::Result<()>
{
    match op {
        Operand::RM(RegM::R(r)) => print!("{:?}", r),
        Operand::RM(m) => print!("{:?}", m),
        Operand::RP(rp) => print!("{:?}", rp),
        Operand::PSW => print!("PSW"),
        Operand::C3(c) => print!("{:02X}H", c * 8),
        Operand::I8 | Operand::Port8 => if let Some((_, i)) = f.next() {
            print!("{:02X}H", i?)
        } else {
            print!("???")
        },
        Operand::I16 | Operand::Addr => if let Some((_, lo)) = f.next() {
            if let Some((_, hi)) = f.next() {
                print!("{:04X}H", (hi? as u16) << 8 | (lo? as u16))
            } else {
                print!("???")
            }
        } else {
            print!("???")
        },
    }
    Ok(())
}
