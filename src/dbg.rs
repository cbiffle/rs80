//! Emulated debug monitor.

use std::io::{self, prelude::*};
use std::collections::HashMap;
use std::fs;

use rs80_common::isa::{Reg, RegPair};
use super::emu;
use super::dis;

/// State of the monitor, including the machine under test.
#[derive(Clone)]
pub struct Mon {
    /// Machine under test.
    machine: emu::Emu,
    /// Addresses with breakpoints. This helps distinguish a monitor-induced HLT
    /// from an actual HLT in the program.
    breakpoints: HashMap<u16, Breakpoint>,
    /// I/O handler.
    io: ConversationPorts,
}

impl Default for Mon {
    fn default() -> Self {
        Mon {
            machine: emu::Emu::default(),
            breakpoints: HashMap::default(),
            io: ConversationPorts,
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
        let inp = io::stdin();

        println!("8080 Debug Monitor: starting.");

        let mut cmd = String::new();

        loop {
            cmd.clear();
            self.print_info();
            print!("> ");
            io::stdout().flush()?;

            {
                let mut inp = inp.lock();
                let n = inp.read_line(&mut cmd)?;
                if n == 0 { return Ok(()) }
            }
            
            let words: Vec<_> = cmd.split_whitespace().collect();
            if words.is_empty() { continue }

            match words[0] {
                "d" => self.disassemble()?,
                "l" => if words.len() == 3 {
                    self.load_file(words[1], words[2])?;
                } else {
                    println!("Usage: l <filename> <addr>")
                },
                "r" => if words.len() == 3 {
                    self.set_reg(words[1], words[2]);
                } else {
                    println!("Usage: r <reg> <val>")
                },
                "j" => if words.len() == 2 {
                    self.jump(words[1])
                } else {
                    println!("Usage: j <addr>")
                },
                "s" => self.go_step(true),
                "g" => self.go_step(false),
                "b" => self.breakpoint_cmd(&words),
                "@" => self.ram_cmd(&words),
                _ => println!("what"),
            }
        }
    }

    fn print_info(&self) {
        println!(" A  B  C  D  E  H  L   SP   PC FLAGS");
        println!("{:02X} {:02X} {:02X} {:02X} {:02X} {:02X} {:02X} \
                     {:04X} {:04X} {}{}{}{}{}",
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
               if self.machine.flags.parity() { 'P' } else { '-' },
               if self.machine.flags.aux()  { 'A' } else { '-' },
               if self.machine.flags.zero   { 'Z' } else { '-' },
               if self.machine.flags.sign   { 'S' } else { '-' })
    }
    
    fn disassemble(&self) -> io::Result<()> {
        let mut addr = self.machine.get_pc() as usize;
        let final_addr = addr + 16;
        let mut bytes = self.machine.mem.iter()
            .map(|&b| Ok(b))
            .cycle().skip(addr as usize);
        let out = io::stdout();
        while addr < final_addr {
            let mut out = out.lock();
            write!(out, "\t{:02X}\t", addr)?;
            addr += dis::disassemble(&mut bytes, &mut out)?;
            writeln!(out, "")?;
        }
        Ok(())
    }

    fn load_file(&mut self,
                 filename: &str,
                 addr: &str) -> io::Result<()> {
        let addr = match u16::from_str_radix(addr, 16) {
            Err(e) => {
                println!("Bad address: {:?} {:?}", addr, e);
                return Ok(())
            },
            Ok(a) => a,
        };

        let mut f = fs::File::open(filename)?;
        let meta = f.metadata()?;
        if meta.len() > 0x10000 || (meta.len() + (addr as u64)) > 0x10000 {
            println!("Cannot load {}-byte file to address {:04X}",
                          meta.len(), addr);
            return Ok(())
        }

        let addr = addr as usize;
        let last_addr = addr + meta.len() as usize;
        f.read_exact(&mut self.machine.mem[addr .. last_addr])
    }

    fn set_reg(&mut self,
               name: &str,
               val: &str) {
        match name {
            "a" | "A" => self.set_reg8(Reg::A, val),
            "b" | "B" => self.set_reg8(Reg::B, val),
            "c" | "C" => self.set_reg8(Reg::C, val),
            "d" | "D" => self.set_reg8(Reg::D, val),
            "e" | "E" => self.set_reg8(Reg::E, val),
            "h" | "H" => self.set_reg8(Reg::H, val),
            "l" | "L" => self.set_reg8(Reg::L, val),

            "bc" | "BC" => self.set_reg_pair(RegPair::BC, val),
            "de" | "DE" => self.set_reg_pair(RegPair::DE, val),
            "hl" | "HL" => self.set_reg_pair(RegPair::HL, val),
            "sp" | "SP" => self.set_reg_pair(RegPair::SP, val),

            _ => println!("Bad register name: {}", name),
        }
    }

    fn set_reg8(&mut self,
                reg: Reg,
                val: &str) {
        let val = match u8::from_str_radix(val, 16) {
            Err(e) => {
                println!("Bad 8-bit value: {:?} {:?}", val, e);
                return
            },
            Ok(a) => a,
        };
        self.machine.set_reg(reg, val);
    }

    fn set_reg_pair(&mut self,
                    reg: RegPair,
                    val: &str) {
        let val = match u16::from_str_radix(val, 16) {
            Err(e) => {
                println!("Bad 16-bit value: {:?} {:?}", val, e);
                return 
            },
            Ok(a) => a,
        };
        self.machine.set_reg_pair(reg, val);
    }

    fn jump(&mut self,
            addr: &str) {
        let addr = match u16::from_str_radix(addr, 16) {
            Err(e) => {
                println!("Bad address: {:?} {:?}", addr, e);
                return
            },
            Ok(a) => a,
        };
        self.machine.jump(addr);
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
               step: bool) {
        let starting_pc = self.machine.get_pc();
        if self.breakpoints.contains_key(&starting_pc) {
            // After stopping at a breakpoint, we want stepping or running again
            // to succeed. So we will step without installing breakpoints just
            // yet.
            match emu::step(&mut self.machine, &mut self.io) {
                // Halted anyway -- program literally contains a HLT here.
                Ok(true) => println!("(halt)"),
                Ok(_) => (),
                Err(emu::RunError::UnimplementedInstruction(op, addr)) => {
                    println!("UNIMPLEMENTED {:02X} @{:04X}", op, addr);
                    return
                }
            }
            // We may have arrived at a new breakpoint; report it if so.
            if self.breakpoints.contains_key(&self.machine.get_pc()) {
                println!("Breakpoint @{:04X}", self.machine.get_pc())
            }
            // If we were only stepping, then we're now done.
            if step { return }
        }

        // Single step of non-breakpoint or run past breakpoint.
        self.install_breakpoints();
        let r = if step {
            emu::step(&mut self.machine, &mut self.io)
        } else {
            emu::run(&mut self.machine, &mut self.io).map(|_| true)
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
                        self.machine.jump(halt_addr);
                        println!("Breakpoint @{:04X}", halt_addr);
                        return
                    } else {
                        // This was a literal halt instruction in the program.
                        println!("(halt)")
                    }
                }

                if self.breakpoints.contains_key(&self.machine.get_pc()) {
                    println!("Breakpoint @{:04X}", self.machine.get_pc())
                }
            },
            Err(emu::RunError::UnimplementedInstruction(op, addr)) =>
                println!("UNIMPLEMENTED {:02X} @{:04X}", op, addr),
        }
    }

    fn breakpoint_cmd(&mut self, words: &[&str]) {
        match words.len() {
            1 => {
                println!("Breakpoints:");
                for k in self.breakpoints.keys() {
                    println!("\t{:04X}", k)
                }
            },
            2 => {
                let arg = words[1];
                if arg.starts_with("-") {
                    match u16::from_str_radix(&arg[1..], 16) {
                        Err(_) => println!("Bad address: {}", arg),
                        Ok(a) => {
                            let p = self.breakpoints.remove(&a);
                            if p.is_none() {
                                println!("Warning: no such breakpoint")
                            }
                        },
                    }
                } else {
                    match u16::from_str_radix(arg, 16) {
                        Err(_) => println!("Bad address: {}", arg),
                        Ok(a) => {
                            let bp = Breakpoint { orig: 0 };
                            let p = self.breakpoints.insert(a, bp);
                            if p.is_some() {
                                println!("Warning: duplicate")
                            }
                        },
                    }
                }
            },
            _ => println!("Usage:\nb\nb <addr>\nb -<addr>"),
        }
    }

    fn ram_cmd(&mut self, words: &[&str]) {
        if words.len() < 2 || words.len() > 3 {
            return println!("Usage:\n@ <addr>\n@ <addr> <val>")
        }

        let addr = match u16::from_str_radix(words[1], 16) {
            Err(_) => return println!("Bad address: {}", words[1]),
            Ok(a) => a,
        };

        if words.len() == 2 {
            println!("\t{:04X}\t{:02X}\n",
                   addr,
                   self.machine.mem[addr as usize])
        } else {
            let val = match u8::from_str_radix(words[2], 16) {
                Err(_) => return println!("Bad value: {}", words[1]),
                Ok(a) => a,
            };
            self.machine.mem[addr as usize] = val;
        }
    }
}

#[derive(Clone, Debug)]
struct ConversationPorts;

impl emu::Ports for ConversationPorts {
    fn write_port(&mut self, port: u8, val: u8) {
        println!("OUT [{:02X}] <- {:02X}", port, val)
    }
    fn read_port(&mut self, port: u8) -> u8 {
        let inp = io::stdin();
        let mut inp = inp.lock();

        let mut buf = String::new();
        loop {
            buf.clear();
            print!("IN [{:02X}] = ", port);
            io::stdout().flush().unwrap();
            let _ = inp.read_line(&mut buf).unwrap();
            match u8::from_str_radix(buf.trim(), 16) {
                Ok(v) => return v,
                Err(_) => println!("Bad value: {}", buf),
            }
        }
    }
}
