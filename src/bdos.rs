//! BDOS emulation, for running simple CP/M programs without CP/M.

use std::io::{self, Read};
use std::fs;

use rs80_common::isa::{RegPair, Reg};
use super::emu::{Emu, run, Ports};

/// Reads in a COM file from the filesystem and plops it into memory in `emu` at
/// the appropriate CP/M load address (0x100).
pub fn load_image(path: String, emu: &mut Emu) -> io::Result<()> {
    let mut file = fs::File::open(path)?;
    let mut vec = vec![];
    file.read_to_end(&mut vec)?;
    load_image_(&vec, emu);
    Ok(())
}

/// Given an image of a COM file in memory (`data`), plops it into memory in
/// `emu`.
pub fn load_image_(data: &[u8], emu: &mut Emu) {
    let mem = &mut emu.mem[0x100..];
    mem[..data.len()].copy_from_slice(data);
}

/// Things that can go wrong during BDOS emulation.
#[derive(Debug)]
pub enum BdosError {
    /// BDOS call not implemented at address.
    UnhandledBdosCall(u8, u16),
    /// Unexpected halt (i.e. one not used for emulation).
    Halted(u16),
    /// Error writing to console.
    Out(io::Error),
}

/// Scribbles in the first 256 bytes of RAM to set up for BDOS emulation. This
/// is mostly responsible for (1) placing HLTs to catch unexpected branches to
/// unimplemented functions and (2) placing a single RET to cause `CALL 5` to
/// work properly.
pub fn initialize_page_zero(emu: &mut Emu) {
    // Fill low memory with HLTs
    for i in 0..0x100 {
        emu.memory_mut()[i] = 0x76;
    }
    // Place a RET one byte *past* CP/M entry point, to handle syscall
    // emulation. (So we halt and then ret.)
    emu.memory_mut()[5 + 1] = 0xC9;
}

/// Trait implemented by things that emulate BDOS functions.
///
/// TODO: should this just be a FnMut variant?
pub trait BdosCall {
    fn call_function(&mut self,
                     function: u8,
                     emu: &mut Emu)
        -> Result<(), BdosError>;
}

/// Runs `emu` using `ports` for direct I/O, and providing BDOS console
/// emulation directing output to `out`.
///
/// If the program exits (by jumping to 0), returns the address of the jump.
pub fn run_bdos<W>(emu: &mut Emu,
                   ports: &mut impl Ports,
                   out: W)
    -> Result<u16, BdosError>
    where W: io::Write,
{
    let mut calls = ConsoleOnly(out);
    run_bdos_custom(emu, ports, &mut calls)
}

/// Runs `emu` using `ports` for direct I/O, and routing BDOS calls to `calls`.
///
/// If the program exits (by jumping to 0), returns the address of the jump.
pub fn run_bdos_custom(emu: &mut Emu,
                       ports: &mut impl Ports,
                       calls: &mut dyn BdosCall)
    -> Result<u16, BdosError>
{
    // Start at 0x100 like CP/M.
    emu.jump(0x100);
    // We've got a full 64kiB of memory, so aim the stack pointer at the bottom
    // of it so the first word pushed lands at FFFEH.
    emu.set_reg_pair(RegPair::SP, 0);

    loop {
        let (last_pc, halt_addr) = run(emu, ports);
        match halt_addr {
            5 => { // CP/M syscall restart address
                let function = emu.reg(Reg::C);
                calls.call_function(function, emu)?
            },
            0 => {
                // Jump to exit vector
                return Ok(last_pc)
            },
            _ => return Err(BdosError::Halted(halt_addr))
        }
    }
}

/// Simple implementation of `BdosCall` providing only console output.
pub struct ConsoleOnly<W>(pub W);

impl<W> BdosCall for ConsoleOnly<W>
    where W: io::Write
{
    fn call_function(&mut self,
                     function: u8,
                     emu: &mut Emu) -> Result<(), BdosError> {
        match function {
            2 => {
                // Put character
                let c = emu.reg(Reg::E);
                self.0.write(&[c])
                    .map_err(BdosError::Out)?;
            },
            9 => {
                // Type string
                let mut addr = emu.reg_pair(RegPair::DE);
                loop {
                    let c = emu.load(addr);
                    if c == b'$' { break }
                    self.0.write(&[c])
                        .map_err(BdosError::Out)?;
                    addr = addr.wrapping_add(1);
                }
            },
            _ => {
                let retaddr = emu.load16(emu.reg_pair(RegPair::SP));
                return Err(BdosError::UnhandledBdosCall(function, retaddr))
            },
        }
        Ok(())
    }
}
