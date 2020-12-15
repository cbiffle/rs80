//! I needed an assembler that could process late-1970s Digital Research
//! assembler syntax. Writing one seemed error prone. Instead, here is a
//! program-specific emulator that runs the assembler from CP/M 2.2, which
//! I obtained from the internets.

use rs80_common::isa::{Reg, RegPair};
use rs80::emu::{Emu, RunError};
use rs80::bdos::*;

use std::io;
use std::fs;

static PROGRAM: &[u8] = include_bytes!("ASM.COM");

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    args.next();

    let source_name = args.next().expect("Missing source filename");
    let listing_name = args.next().expect("Missing listing filename");
    let hex_name = args.next().expect("Missing hex filename");

    let source = fs::File::open(source_name)?;
    let listing = fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(listing_name)?;
    let hex = fs::OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(hex_name)?;

    let mut emu = Emu::default();
    emu.mem[0x100 .. 0x100 + PROGRAM.len()].copy_from_slice(PROGRAM);

    let out = io::stdout();
    let mut out = out.lock();
    initialize_page_zero(&mut emu);

    // Establish the CCP FCB containing fake arguments.
    for addr in emu.memory_mut()[0x5C .. 0x5C + 16].iter_mut() {
        *addr = b' ';
    }
    emu.memory_mut()[0x5C] = 0;  // Drive.
    emu.memory_mut()[0x5D .. 0x5D + 8].copy_from_slice(BASENAME);

    let mut sim = BdosSim::new(ConsoleOnly(&mut out), source, listing, hex);

    match run_bdos_custom(&mut emu, &mut (), &mut sim) {
        Ok(_) => (),
        Err(BdosError::UnhandledBdosCall(c, pc)) =>
            println!("\nERROR: unhandled BDOS call {:X}H at {:04X}", c, pc),
        Err(BdosError::Halted(pc)) =>
            println!("\nHALTED at {:04X}", pc),
        Err(BdosError::Out(e)) => return Err(e),
        Err(BdosError::RunError(RunError::UnimplementedInstruction(op, pc))) =>
            println!("\nERROR: unimplemented: {:02X} at {:04X}", op, pc),
    }

    Ok(())
}

pub struct BdosSim<B, I, O> {
    inner: B,
    current_drive: u8,
    dma_addr: u16,
    source: I,
    listing: O,
    hex: O,
}

impl<B, I, O> BdosSim<B, I, O> {
    fn new(inner: B, source: I, listing: O, hex: O) -> Self {
        BdosSim {
            inner,
            current_drive: 0,
            dma_addr: 0x80,
            source, listing, hex
        }
    }

    fn check_fcb(&self, emu: &mut Emu) -> Option<Files> {
        let fcb_addr = emu.reg_pair(RegPair::DE) as usize;
        let fcb = &emu.memory()[fcb_addr .. fcb_addr + 33];
        let drive = fcb[0];
        let basename = &fcb[1..=8];
        let ext = &fcb[9..=11];

        let nonexistent_drive = drive > 1 || (drive == 0 && self.current_drive != 0);
        let odd_access = basename != BASENAME;
        if nonexistent_drive || odd_access {
            // Go away.
            None
        } else if ext == b"ASM" {
            Some(Files::Asm)
        } else if ext == b"PRN" {
            Some(Files::Prn)
        } else if ext == b"HEX" {
            Some(Files::Hex)
        } else {
            None
        }
    }

    fn get_fcb_offset(&self, emu: &mut Emu) -> u64 {
        let fcb_addr = emu.reg_pair(RegPair::DE) as usize;
        let fcb = &emu.memory()[fcb_addr .. fcb_addr + 33];
        let extent = fcb[12];
        let cr = fcb[32];

        ((extent as u64 * 128) + cr as u64) * 128
    }

    fn set_fcb_offset(&mut self, emu: &mut Emu, offset: u64) {
        let fcb_addr = emu.reg_pair(RegPair::DE) as usize;
        let fcb = &mut emu.memory_mut()[fcb_addr .. fcb_addr + 33];
        
        let record = offset / 128;
        let cr = record % 128;
        let extent = record / 128;

        fcb[12] = extent as u8;
        fcb[32] = cr as u8;
    }
}

static BASENAME: &[u8] = b"BASENAME";

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Files {
    Asm,
    Prn,
    Hex,
}

impl<B, I, O> BdosCall for BdosSim<B, I, O>
    where B: BdosCall,
          I: io::Read + io::Seek,
          O: io::Write + io::Seek
{
    fn call_function(&mut self,
                     function: u8,
                     emu: &mut Emu) -> Result<(), BdosError> {
        match function {
            14 => {
                // Select drive. Drive in E. This has no way of indicating
                // errors.
                let drive = emu.reg(Reg::E);
                if drive > 15 {
                    panic!("Bad Select Disk: {}", drive)
                }
                self.current_drive = drive;
                Ok(())
            },
            15 => {
                // Open file. There are only three files required for the
                // assembler. Check if this is one of them.
                let result = self.check_fcb(emu).map(|_| 0).unwrap_or(0xFF);
                emu.set_reg(Reg::A, result);
                Ok(())
            },
            16 => {
                // Close file. No action required on our part.
                emu.set_reg(Reg::A, 0);
                Ok(())
            },
            19 => {
                // Delete file. ASM.COM deletes its output before writing, so
                // just permit it if the filename is reasonable.
                let result = self.check_fcb(emu).map(|_| 0).unwrap_or(0xFF);
                emu.set_reg(Reg::A, result);
                Ok(())
            },
            20 => {
                // Read sequential. We only allow reads from the ASM source
                // file, so check the access.
                if let Some(Files::Asm) = self.check_fcb(emu) {
                    // We'll read directly into target RAM. Our "DMA controller"
                    // does not "support" accesses that wrap the top of memory.
                    // Fortunately ASM does not do such accesses.
                    let offset = self.get_fcb_offset(emu);
                    self.source.seek(io::SeekFrom::Start(offset))
                        .map_err(BdosError::Out)?;

                    {
                        let dest =
                            &mut emu.memory_mut()[self.dma_addr as usize ..
                                         self.dma_addr as usize + 128];
                        let mut num_read = 0;
                        while num_read < 128 {
                            let count = self.source.read(&mut dest[num_read..])
                                .map_err(BdosError::Out)?;
                            if count == 0 {
                                for r in dest[num_read..].iter_mut() {
                                    *r = 0x1A; // EOF character
                                }
                                break
                            }
                            num_read += count;
                        }
                    }
                    self.set_fcb_offset(emu, offset + 128);
                    emu.set_reg(Reg::A, 0);
                    Ok(())
                } else {
                    emu.set_reg(Reg::A, 0xFF);
                    Ok(())
                }
            },
            21 => {
                // Write sequential. Only the listing and hex files can be
                // written.
                match self.check_fcb(emu) {
                    Some(Files::Asm) => {
                        // This operating system has no notion of a file not
                        // opened for write, so we'll indicate disk full.
                        emu.set_reg(Reg::A, 0xFF);
                        Ok(())
                    },
                    Some(f) => {
                        let offset = self.get_fcb_offset(emu);
                        self.source.seek(io::SeekFrom::Start(offset))
                            .map_err(BdosError::Out)?;

                        {
                            let src =
                                &emu.memory_mut()[self.dma_addr as usize ..
                                    self.dma_addr as usize + 128];

                            let dest = if f == Files::Prn {
                                &mut self.listing
                            } else {
                                &mut self.hex
                            };

                            dest.write_all(src)
                                .map_err(BdosError::Out)?;
                        }

                        self.set_fcb_offset(emu, offset + 128);
                        emu.set_reg(Reg::A, 0);
                        Ok(())
                    },
                    _ => {
                        emu.set_reg(Reg::A, 0xFF);
                        Ok(())
                    },
                }
            },
            22 => {
                // Make file. Sure.
                let result = self.check_fcb(emu).map(|_| 0).unwrap_or(0xFF);
                emu.set_reg(Reg::A, result);
                Ok(())
            },
            25 => {
                // Get drive.
                emu.set_reg(Reg::A, self.current_drive);
                Ok(())
            },
            _ => self.inner.call_function(function, emu)
        }
    }
}

