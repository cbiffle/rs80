//! Emulated debug monitor.

use std::io::{self, prelude::*};
use std::collections::HashMap;
use std::fs;
use std::num::Wrapping;

use super::isa::{Reg, RegPair, Opcode};
use super::emu;
use super::dis;
use super::ops;

/// State of the monitor, including the machine under test.
#[derive(Clone, Debug)]
pub struct Mon {
    /// Machine under test.
    machine: emu::Emu,
    /// Addresses with breakpoints. This helps distinguish a monitor-induced HLT
    /// from an actual HLT in the program.
    breakpoints: HashMap<u16, Breakpoint>,
}

impl Default for Mon {
    fn default() -> Self {
        Mon {
            machine: emu::Emu::default(),
            breakpoints: HashMap::default(),
        }
    }
}

/// Information tracked for each breakpoint.
#[derive(Copy, Clone, Debug)]
struct Breakpoint {
    /// Byte that was removed from the program to create this breakpoint.
    orig: u8,
}

const HLT: u8 = 0x76;

impl Mon {
    pub fn run(&mut self) -> io::Result<()> {
        let out = io::stdout();
        let mut out = out.lock();

        let inp = io::stdin();
        let mut inp = inp.lock();

        write!(&mut out, "8080 Debug Monitor: starting.\n")?;

        let mut cmd = String::new();

        loop {
            cmd.clear();
            self.print_info(&mut out)?;
            write!(&mut out, "> ")?;
            out.flush()?;
            let n = inp.read_line(&mut cmd)?;
            if n == 0 { return Ok(()) }
            
            let words: Vec<_> = cmd.split_whitespace().collect();
            if words.is_empty() { continue }

            match words[0] {
                "d" => self.disassemble(&mut out)?,
                "l" => if words.len() == 3 {
                    self.load_file(words[1], words[2], &mut out)?;
                } else {
                    write!(&mut out, "Usage: l <filename> <addr>\n")?
                },
                "r" => if words.len() == 3 {
                    self.set_reg(words[1], words[2], &mut out)?;
                } else {
                    write!(&mut out, "Usage: r <reg> <val>\n")?
                },
                "j" => if words.len() == 2 {
                    self.jump(words[1], &mut out)?
                } else {
                    write!(&mut out, "Usage: j <addr>\n")?
                },
                "s" => self.go_step(true, &mut out)?,
                "g" => self.go_step(false, &mut out)?,
                "b" => self.breakpoint_cmd(&words, &mut out)?,
                "@" => self.ram_cmd(&words, &mut out)?,
                _ => write!(&mut out, "what\n")?,
            }
        }
    }

    fn print_info(&self, out: &mut impl Write) -> io::Result<()> {
        write!(out, " A  B  C  D  E  H  L   SP   PC FLAGS\n")?;
        write!(out, "{:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} \
                     {:04X} {:04X} {}{}{}{}{}\n",
               self.machine.reg(Reg::A),
               self.machine.reg(Reg::B),
               self.machine.reg(Reg::C),
               self.machine.reg(Reg::D),
               self.machine.reg(Reg::E),
               self.machine.reg(Reg::H),
               self.machine.reg(Reg::L),
               self.machine.reg_pair(RegPair::SP),
               self.machine.get_pc(),
               if self.machine.flags.carry  { 'C' } else { '-' },
               if self.machine.flags.parity { 'P' } else { '-' },
               if self.machine.flags.aux    { 'A' } else { '-' },
               if self.machine.flags.zero   { 'Z' } else { '-' },
               if self.machine.flags.sign   { 'S' } else { '-' })
    }
    
    fn disassemble(&self, out: &mut impl Write) -> io::Result<()> {
        let mut addr = self.machine.get_pc() as usize;
        let final_addr = addr + 16;
        let mut bytes = self.machine.mem.iter()
            .map(|&b| Ok(b))
            .cycle().skip(addr as usize);
        while addr < final_addr {
            write!(out, "\t{:02X}\t", addr)?;
            addr += dis::disassemble(&mut bytes, out)?;
            write!(out, "\n")?;
        }
        Ok(())
    }

    fn load_file(&mut self,
                 filename: &str,
                 addr: &str,
                 out: &mut impl Write) -> io::Result<()> {
        let addr = match u16::from_str_radix(addr, 16) {
            Err(e) => {
                return write!(out, "Bad address: {:?} {:?}", addr, e)
            },
            Ok(a) => a,
        };

        let mut f = fs::File::open(filename)?;
        let meta = f.metadata()?;
        if meta.len() > 0x10000 || (meta.len() + (addr as u64)) > 0x10000 {
            return write!(out, "Cannot load {}-byte file to address {:04X}",
                          meta.len(), addr)
        }

        let addr = addr as usize;
        let last_addr = addr + meta.len() as usize;
        f.read_exact(&mut self.machine.mem[addr .. last_addr])
    }

    fn set_reg(&mut self,
               name: &str,
               val: &str,
               out: &mut impl Write) -> io::Result<()> {
        match name {
            "a" | "A" => self.set_reg8(Reg::A, val, out),
            "b" | "B" => self.set_reg8(Reg::B, val, out),
            "c" | "C" => self.set_reg8(Reg::C, val, out),
            "d" | "D" => self.set_reg8(Reg::D, val, out),
            "e" | "E" => self.set_reg8(Reg::E, val, out),
            "h" | "H" => self.set_reg8(Reg::H, val, out),
            "l" | "L" => self.set_reg8(Reg::L, val, out),

            "bc" | "BC" => self.set_reg_pair(RegPair::BC, val, out),
            "de" | "DE" => self.set_reg_pair(RegPair::DE, val, out),
            "hl" | "HL" => self.set_reg_pair(RegPair::HL, val, out),
            "sp" | "SP" => self.set_reg_pair(RegPair::SP, val, out),

            _ => write!(out, "Bad register name: {}", name),
        }
    }

    fn set_reg8(&mut self,
                reg: Reg,
                val: &str,
                out: &mut impl Write) -> io::Result<()> {
        let val = match u8::from_str_radix(val, 16) {
            Err(e) => {
                return write!(out, "Bad 8-bit value: {:?} {:?}", val, e)
            },
            Ok(a) => a,
        };
        self.machine.set_reg(reg, Wrapping(val));
        Ok(())
    }

    fn set_reg_pair(&mut self,
                    reg: RegPair,
                    val: &str,
                    out: &mut impl Write) -> io::Result<()> {
        let val = match u16::from_str_radix(val, 16) {
            Err(e) => {
                return write!(out, "Bad 16-bit value: {:?} {:?}", val, e)
            },
            Ok(a) => a,
        };
        self.machine.set_reg_pair(reg, Wrapping(val));
        Ok(())
    }

    fn jump(&mut self,
            addr: &str,
            out: &mut impl Write) -> io::Result<()> {
        let addr = match u16::from_str_radix(addr, 16) {
            Err(e) => {
                return write!(out, "Bad address: {:?} {:?}", addr, e)
            },
            Ok(a) => a,
        };
        self.machine.jump(Wrapping(addr));
        Ok(())
    }

    fn install_breakpoints(&mut self) {
        for (addr, bp) in self.breakpoints.iter_mut() {
            let addr = *addr as usize;
            // Back up the program byte at the breakpoint address.
            bp.orig = self.machine.mem[addr];
            // Substitute it for a HLT.
            self.machine.mem[addr] = HLT;
        }
    }

    fn remove_breakpoints(&mut self) {
        for (addr, bp) in self.breakpoints.iter_mut() {
            let addr = *addr as usize;
            // Restore the program byte.
            self.machine.mem[addr] = bp.orig;
        }
    }

    fn go_step(&mut self,
               step: bool,
               out: &mut impl Write) -> io::Result<()> {
        let starting_pc = self.machine.get_pc();
        if self.breakpoints.contains_key(&starting_pc) {
            // After stopping at a breakpoint, we want stepping or running again
            // to succeed. So we will step without installing breakpoints just
            // yet.
            match emu::step(&mut self.machine) {
                // Halted anyway -- program literally contains a HLT here.
                Ok(true) => {
                    write!(out, "(halt)\n")?
                },
                Ok(_) => (),
                Err(emu::RunError::UnimplementedInstruction(op, addr)) => {
                    write!(out, "UNIMPLEMENTED {:02X} @{:04X}\n", op, addr)?;
                    return Ok(())
                }
            }
            // We may have arrived at a new breakpoint; report it if so.
            if self.breakpoints.contains_key(&self.machine.get_pc()) {
                write!(out, "Breakpoint @{:04X}\n", self.machine.get_pc())?;
            }
            // If we were only stepping, then we're now done.
            if step { return Ok(()) }
        }

        // Single step of non-breakpoint or run past breakpoint.
        self.install_breakpoints();
        let r = if step {
            emu::step(&mut self.machine)
        } else {
            emu::run(&mut self.machine).map(|_| true)
        };
        self.remove_breakpoints();
        match r {
            Ok(halted) => {
                if halted {
                    let halt_addr = self.machine.get_pc() - 1;
                    // Check if there is a breakpoint at this address. If so, we
                    // need to prepare to re-execute the instruction if we
                    // halted.
                    if self.breakpoints.contains_key(&halt_addr) {
                        // Back up to the breakpoint.
                        self.machine.jump(Wrapping(halt_addr));
                        write!(out, "Breakpoint @{:04X}\n", halt_addr)?;
                        return Ok(())
                    } else {
                        // This was a literal halt instruction in the program.
                        write!(out, "(halt)\n")?
                    }
                }

                if self.breakpoints.contains_key(&self.machine.get_pc()) {
                    write!(out, "Breakpoint @{:04X}\n", self.machine.get_pc())?;
                }

                Ok(())
            },
            Err(emu::RunError::UnimplementedInstruction(op, addr)) =>
                write!(out, "UNIMPLEMENTED {:02X} @{:04X}\n", op, addr),
        }
    }

    fn breakpoint_cmd(&mut self,
                      words: &[&str],
                      out: &mut impl Write) -> io::Result<()> {
        match words.len() {
            1 => {
                write!(out, "Breakpoints:\n")?;
                for k in self.breakpoints.keys() {
                    write!(out, "\t{:04X}\n", k)?
                }
                Ok(())
            },
            2 => {
                let arg = words[1];
                if arg.starts_with("-") {
                    match u16::from_str_radix(&arg[1..], 16) {
                        Err(_) => write!(out, "Bad address: {}\n", arg)?,
                        Ok(a) => {
                            let p = self.breakpoints.remove(&a);
                            if p.is_none() {
                                write!(out, "Warning: no such breakpoint\n")?
                            }
                        },
                    }
                    Ok(())
                } else {
                    match u16::from_str_radix(arg, 16) {
                        Err(_) => write!(out, "Bad address: {}\n", arg),
                        Ok(a) => {
                            let bp = Breakpoint { orig: 0 };
                            let p = self.breakpoints.insert(a, bp);
                            if p.is_some() {
                                write!(out, "Warning: duplicate\n")?
                            }
                            Ok(())
                        },
                    }
                }
            },
            _ => write!(out, "Usage:\nb\nb <addr>\nb -<addr>\n"),
        }
    }
    fn ram_cmd(&mut self,
               words: &[&str],
               out: &mut impl Write) -> io::Result<()> {
        if words.len() < 2 || words.len() > 3 {
            return write!(out, "Usage:\n@ <addr>\n@ <addr> <val>\n")
        }

        let addr = match u16::from_str_radix(words[1], 16) {
            Err(_) => return write!(out, "Bad address: {}\n", words[1]),
            Ok(a) => a,
        };

        if words.len() == 2 {
            write!(out, "\t{:04X}\t{:02X}\n",
                   addr,
                   self.machine.mem[addr as usize])
        } else {
            let val = match u8::from_str_radix(words[2], 16) {
                Err(_) => return write!(out, "Bad value: {}\n", words[1]),
                Ok(a) => a,
            };
            self.machine.mem[addr as usize] = val;
            Ok(())
        }
    }
}
