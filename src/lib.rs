/// A simple emulator for the 8080 (or, really, a clone thereof -- this is
/// tested against an emulator for the Soviet KR580VM80A).

#[macro_use]
extern crate lazy_static;

mod lazy;
pub mod isa;
pub mod emu;
pub mod ops;

use std::num::Wrapping;
use std::io::{self, Read};

// TODO
use isa::Reg as Reg8;
use isa::RegPair;
use isa::Opcode;

pub use emu::Emu;
pub use ops::make_decode_table;
// TODO

pub fn load_image(path: String, emu: &mut Emu) -> io::Result<()> {
    let mut file = std::fs::File::open(path)?;
    file.read(&mut emu.mem[0x100..])?;
    Ok(())
}

#[derive(Debug)]
pub enum RunError {
    UnhandledBdosCall(u8, u16),
    BadJump(u16, u16),
    UnimplementedInstruction(u8, u16),
    Halted(u16),
    Out(io::Error),
}

pub fn run<W>(emu: &mut Emu,
              mut out: W)
    -> Result<u16, RunError>
    where W: io::Write
{
    // Start at 0x100 like CP/M.
    emu.jump(Wrapping(0x100));
    emu.set_reg_pair(RegPair::SP, Wrapping(0));
    emu.clear_halt_flag();
    // Place a RET to handle CP/M restarts.
    emu.memory_mut()[5] = 0xC9;

    let table: &[Option<_>; 256] = &ops::DISPATCH;
    let mut last_pc = 0;

    let mut ctx = ops::Ctx { io: &mut () };

    while !emu.is_halted() {
        if emu.get_pc() < 0x100 {
            match emu.get_pc() {
                5 => {
                    // CP/M syscall restart address
                    match emu.reg(Reg8::C).0 {
                        9 => {
                            // Type string
                            let mut addr = emu.reg_pair(RegPair::DE);
                            loop {
                                let c = emu.load(addr).0;
                                if c == b'$' { break }
                                out.write(&[c])
                                    .map_err(RunError::Out)?;
                                addr += Wrapping(1);
                            }
                        },
                        2 => {
                            let c = emu.reg(Reg8::E).0;
                            out.write(&[c])
                                    .map_err(RunError::Out)?;
                        },
                        _ => return Err(RunError::UnhandledBdosCall(
                                emu.reg(Reg8::C).0, last_pc)),
                    }
                },
                0 => {
                    // Jump to exit vector
                    return Ok(last_pc)
                },
                _ => return Err(RunError::BadJump(emu.get_pc(), last_pc)),
            }
        }

        last_pc = emu.get_pc();

        let op = emu.take_imm8().0;
        match table[op as usize] {
            None => return Err(RunError::UnimplementedInstruction(
                    op, last_pc)),
            Some(f) => f(Opcode(op), emu, &mut ctx),
        }
    }

    Err(RunError::Halted(last_pc))
}
