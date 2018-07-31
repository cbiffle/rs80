//! BDOS emulation, for running simple CP/M programs without CP/M.

use std::io::{self, Read};
use std::fs;
use std::path::PathBuf;

use rs80_common::isa::{RegPair, Reg};
use super::emu::{Emu, run, RunError, Ports};

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

pub fn initialize_page_zero(emu: &mut Emu) {
    // Fill low memory with HLTs
    for i in 0..0x100 {
        emu.memory_mut()[i] = 0x76;
    }
    // Place a RET one byte *past* CP/M entry point, to handle syscall
    // emulation. (So we halt and then ret.)
    emu.memory_mut()[5 + 1] = 0xC9;
}

pub struct BdosState {
    current_drive: u8,
}

pub fn run_bdos<W>(emu: &mut Emu,
                   ports: &mut impl Ports,
                   mut out: W,
                   dispatcher: fn(u8, &mut Emu, &mut BdosState, &mut W) ->
                               Result<bool, BdosError>)
    -> Result<u16, BdosError>
    where W: io::Write,
{
    // Start at 0x100 like CP/M.
    emu.jump(0x100);
    emu.set_reg_pair(RegPair::SP, 0);
    // Fill low memory with HLTs
    for i in 0..0x100 {
        emu.memory_mut()[i] = 0x76;
    }
    // Place a RET one byte *past* CP/M entry point, to handle syscall
    // emulation. (So we halt and then ret.)
    emu.memory_mut()[5 + 1] = 0xC9;
    /*
    // Initialize default FCB.
    emu.memory_mut()[0x5C..(0x5C+16)].copy_from_slice(&[
        0,  // drive
        b'M', b'I', b'T', b'S', b'B', b'I', b'O', b'S',  // filename
        b' ', b' ', b' ', // extension
        b' ', b' ', b' ', b' ', // padding
    ]);
    emu.memory_mut()[0x6C..(0x6C+16)].copy_from_slice(&[
        0,  // drive
        b' ', b' ', b' ', b' ', b' ', b' ', b' ', b' ',  // filename
        b' ', b' ', b' ', // extension
        b' ', b' ', b' ', b' ', // padding
    ]);
    */

    let mut state = BdosState {
        current_drive: 0,
    };

    loop {
        match run(emu, &mut ()) {
            Err(e) => return Err(BdosError::RunError(e)),
            Ok((last_pc, halt_addr)) => {
                match halt_addr {
                    5 => { // CP/M syscall restart address
                        let function = emu.reg(Reg::C);
                        let handled = 
                            dispatcher(function, emu, &mut state, &mut out)?;
                        if !handled {
                            return Err(BdosError::UnhandledBdosCall(
                                    function, last_pc))
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

pub fn console_output_only(function: u8,
                           emu: &mut Emu,
                           state: &mut BdosState,
                           mut out: impl io::Write)
-> Result<bool, BdosError> {
    match function {
        2 => {
            // Put character
            let c = emu.reg(Reg::E);
            out.write(&[c])
                .map_err(BdosError::Out)?;
        },
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
        _ => return Ok(false),
    }
    Ok(true)
}

pub fn basic_disk(function: u8,
                  emu: &mut Emu,
                  state: &mut BdosState,
                  mut out: impl io::Write)
-> Result<bool, BdosError> {
    match function {
        14 => {
            // Select drive.
            let drive = emu.reg(Reg::E);
            if drive > 15 {
                panic!("Bad DRV_SET: {}", drive);
            }
            state.current_drive = drive;
            println!("DRV now {}", drive);
            emu.set_reg(Reg::L, 0);
            emu.set_reg(Reg::A, 0);
        },
        15 => {
            {
                let (drv, filename) = get_fcb_de(&emu);
                println!("F_OPEN: {}:{}", drv, filename);
            }
            emu.set_reg(Reg::A, 0x00);
        },
        19 => {
            {
                let (drv, filename) = get_fcb_de(&emu);
                println!("F_DELETE: {}:{}", drv, filename);
            }
            emu.set_reg(Reg::A, 0xFF);
        },
        20 => {
            // Read sequential
            panic!("read sequential");
        },
        22 => {
            {
                let (drv, filename) = get_fcb_de(&emu);
                println!("F_MAKE: {}:{}", drv, filename);
            }
            emu.set_reg(Reg::A, 0);
        },
        25 => {
            // Get current drive.
            emu.set_reg(Reg::A, state.current_drive);
        },
        _ => return console_output_only(function, emu, state, out),
    }
    Ok(true)
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct DriveUser(pub u8);

impl DriveUser {
    fn drive(self) -> u8 {
        self.0 & 0xF
    }
    fn user(self) -> u8 {
        self.0 >> 4
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct CpmPath {
    drive_user: u8,
    basename: [u8; 8],
    ext: [u8; 3],
}

trait Namespace {
    /// Tries to open the file named in `fcb`. On success, fills in `fcb` and
    /// returns `true`. Returns `false` on failure.
    fn open(fcb: &mut Fcb) -> bool;

    /// Closes any resources associated with `fcb`.
    fn close(fcb: &mut Fcb) -> bool;
}

enum NsMapping {
    Path(CpmPath, PathBuf),
    Dir(u8, PathBuf),
}

struct Fcb {
    drive: u8,
    filename: [u8; 8],
    ext: [u8; 3],
    read_only: bool,
    system: bool,
    hidden: bool,
    extent: u8,



}

fn get_fcb_de(emu: &Emu) -> (u8, String) {
    let fcb = emu.reg_pair(RegPair::DE) as usize;
    let drv = emu.memory()[fcb];
    let filename = &emu.memory()[fcb + 1 .. fcb + 12];
    (drv, String::from_utf8_lossy(filename).into_owned())
}
