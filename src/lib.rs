/// A simple emulator for the 8080 (or, really, a clone thereof -- this is
/// tested against an emulator for the Soviet KR580VM80A).

#[macro_use]
extern crate lazy_static;

pub mod isa;
pub mod emu;
pub mod ops;
pub mod dis;

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

#[derive(Copy, Clone, Debug)]
pub enum RunError {
    /// Instruction not implemented at address.
    UnimplementedInstruction(u8, u16),
}

#[derive(Debug)]
pub enum BdosError {
    /// An underlying emulation error.
    RunError(RunError),
    /// BDOS call not implemented at address.
    UnhandledBdosCall(u8, u16),
    /// Unexpected halt (i.e. one not used for emulation).
    Halted(u16),
    /// Error writing to console.
    Out(io::Error),
}

/// Runs the emulation until the machine encounters a `HLT` (or an illegal
/// instruction).
pub fn run(emu: &mut Emu) -> Result<(u16, u16), RunError> {
    let table: &[Option<_>; 256] = &ops::DISPATCH;
    let mut pc = 0xFFFF;
    let mut last_pc;

    let mut ctx = ops::Ctx { io: &mut () };

    loop {
        last_pc = pc;
        pc = emu.get_pc();
        let op = emu.take_imm8().0;
        let halted = match table[op as usize] {
            None => return Err(RunError::UnimplementedInstruction(
                    op, last_pc)),
            Some(f) => f(Opcode(op), emu, &mut ctx),
        };
        if halted { return Ok((last_pc, emu.get_pc() - 1)) }
    }
}

pub fn run_bdos<W>(emu: &mut Emu,
                   mut out: W)
    -> Result<u16, BdosError>
    where W: io::Write
{
    // Start at 0x100 like CP/M.
    emu.jump(Wrapping(0x100));
    emu.set_reg_pair(RegPair::SP, Wrapping(0));
    // Fill low memory with HLTs
    for i in 0..0x100 {
        emu.memory_mut()[i] = 0x76;
    }
    // Place a RET one byte *past* CP/M entry point, to handle syscall
    // emulation.
    emu.memory_mut()[5 + 1] = 0xC9;

    loop {
        match run(emu) {
            Err(e) => return Err(BdosError::RunError(e)),
            Ok((last_pc, halt_addr)) => {
                match halt_addr {
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
                                        .map_err(BdosError::Out)?;
                                    addr += Wrapping(1);
                                }
                            },
                            2 => {
                                // Put character
                                let c = emu.reg(Reg8::E).0;
                                out.write(&[c])
                                    .map_err(BdosError::Out)?;
                            },
                            _ => return Err(BdosError::UnhandledBdosCall(
                                    emu.reg(Reg8::C).0, last_pc)),
                        }
                    },
                    0 => {
                        // Jump to exit vector
                        return Ok(last_pc)
                    },
                    _ => return Err(BdosError::Halted(halt_addr))
                }
            },
        }
    }
}
