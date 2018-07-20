//! BDOS emulation, for running simple CP/M programs without CP/M.

use std::io::{self, Read};
use std::fs;

use super::isa::{RegPair, Reg};
use super::emu::{Emu, run, RunError};

pub fn load_image(path: String, emu: &mut Emu) -> io::Result<()> {
    let mut file = fs::File::open(path)?;
    file.read(&mut emu.mem[0x100..])?;
    Ok(())
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

pub fn run_bdos<W>(emu: &mut Emu,
                   mut out: W)
    -> Result<u16, BdosError>
    where W: io::Write
{
    // Start at 0x100 like CP/M.
    emu.jump(0x100);
    emu.set_reg_pair(RegPair::SP, 0);
    // Fill low memory with HLTs
    for i in 0..0x100 {
        emu.memory_mut()[i] = 0x76;
    }
    // Place a RET one byte *past* CP/M entry point, to handle syscall
    // emulation.
    emu.memory_mut()[5 + 1] = 0xC9;

    loop {
        match run(emu, &mut ()) {
            Err(e) => return Err(BdosError::RunError(e)),
            Ok((last_pc, halt_addr)) => {
                match halt_addr {
                    5 => {
                        // CP/M syscall restart address
                        match emu.reg(Reg::C) {
                            9 => {
                                // Type string
                                let mut addr = emu.reg_pair(RegPair::DE);
                                loop {
                                    let c = emu.load(addr);
                                    if c == b'$' { break }
                                    out.write(&[c])
                                        .map_err(BdosError::Out)?;
                                    addr = addr.wrapping_add(1);
                                }
                            },
                            2 => {
                                // Put character
                                let c = emu.reg(Reg::E);
                                out.write(&[c])
                                    .map_err(BdosError::Out)?;
                            },
                            _ => return Err(BdosError::UnhandledBdosCall(
                                    emu.reg(Reg::C), last_pc)),
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
