
use super::isa::{Opcode, Reg, RegPair};
use super::dis::{InsnInfo, Operand};
use super::emu::{Emu, Flags, Ports};

////////////////////////////////////////////////////////////////////////////////

fn add(a: u8, b: u8, c: u8, flags: &mut Flags) -> u8 {
    // This addition can't overflow because of the zero-extension.
    let sum16 = a as u16 + b as u16 + c as u16;
   
    let sum8 = sum16 as u8; // may get truncated here though, and that's OK
    flags.zero = sum8 == 0;
    flags.carry = sum16 & 0x100 != 0;
    flags.sign = sum8 & 0x80 != 0;
    flags.aux = ((a & 0xF) + (b & 0xF) + c) & 0x10 != 0;
    flags.parity = sum8.count_ones() % 2 == 0;

    sum8
}

fn inc(a: u8, flags: &mut Flags) -> u8 {
    let sum = a.wrapping_add(1);
   
    flags.zero   = sum == 0;
    flags.aux    = (sum & 0xF) == 0;
    flags.parity = sum.count_ones() % 2 == 0;
    flags.sign   = sum & 0x80 != 0;

    sum
}

fn dec(a: u8, flags: &mut Flags) -> u8 {
    let sum = a.wrapping_sub(1);
   
    flags.zero   = sum == 0;
    flags.aux    = (sum & 0xF) != 0xF;
    flags.parity = sum.count_ones() % 2 == 0;
    flags.sign   = sum & 0x80 != 0;

    sum
}

fn and(a: u8, b: u8, flags: &mut Flags) -> u8 {
    let r = a & b;
   
    flags.zero   = r == 0;
    flags.aux    = (a & 0x8) | (b & 0x8) != 0;
    flags.sign   = r & 0x80 != 0;
    flags.parity = r.count_ones() % 2 == 0;
    flags.carry  = false;

    r
}

fn or(a: u8, b: u8, flags: &mut Flags) -> u8 {
    let r = a | b;
   
    flags.zero = r == 0;
    flags.carry = false;
    flags.aux = false;
    flags.sign = r & 0x80 != 0;
    flags.parity = r.count_ones() % 2 == 0;

    r
}

fn xor(a: u8, b: u8, flags: &mut Flags) -> u8 {
    let r = a ^ b;
   
    flags.zero = r == 0;
    flags.carry = false;
    flags.aux = false;
    flags.sign = r & 0x80 != 0;
    flags.parity = r.count_ones() % 2 == 0;

    r
}

fn sub(a: u8, b: u8, c: u8, flags: &mut Flags) -> u8 {
    // We'll do this subtraction at 16 bit width to get access to carry.
    let r16 = (a as u16).wrapping_sub(b as u16).wrapping_sub(c as u16);
   
    let r = r16 as u8;
    flags.zero   = r == 0;
    flags.carry  = r16 & 0x100 != 0;
    flags.sign   = r & 0x80 != 0;
    flags.aux    = ((a & 0xF) + (!b & 0xF) + (1 - c)) & 0x10 != 0;
    flags.parity = r.count_ones() % 2 == 0;

    r
}

fn addsub_template_r(opcode: Opcode,
                     st: &mut Emu,
                     f: impl FnOnce(u8, u8, u8, &mut Flags) -> u8) -> bool {
    let rm = opcode.bits(2,0);
    let a = st.reg(Reg::A);
    let b = st.reg_m(rm);
    let c = match opcode.bit(3) {
        false => 0,
        _     => st.flags.carry as u8,
    };
    let a = f(a, b, c, &mut st.flags);
    st.set_reg(Reg::A, a);
    st.advance(rm.reg_or_m(4, 7))
}

fn addsub_template_i(opcode: Opcode,
                     st: &mut Emu,
                     f: impl FnOnce(u8, u8, u8, &mut Flags) -> u8) -> bool {
    let a = st.reg(Reg::A);
    let b = st.take_imm8();
    let c = match opcode.bit(3) {
        false => 0,
        _     => st.flags.carry as u8,
    };
    let a = f(a, b, c, &mut st.flags);
    st.set_reg(Reg::A, a);
    st.advance(7)
}

fn logic_template_r(opcode: Opcode,
                    st: &mut Emu,
                    f: impl FnOnce(u8, u8, &mut Flags) -> u8) -> bool {
    let rm = opcode.bits(2,0);
    let a = st.reg(Reg::A);
    let b = st.reg_m(rm);
    let a = f(a, b, &mut st.flags);
    st.set_reg(Reg::A, a);
    st.advance(rm.reg_or_m(4, 7))
}

fn logic_template_i(st: &mut Emu,
                    f: impl FnOnce(u8, u8, &mut Flags) -> u8) -> bool {
    let a = st.reg(Reg::A);
    let b = st.take_imm8();
    let a = f(a, b, &mut st.flags);
    st.set_reg(Reg::A, a);
    st.advance(7)
}

fn compare(a: u8, b: u8, flags: &mut Flags) {
    sub(a, b, 0, flags);
}

pub struct Ctx<'a> {
    pub io: &'a mut Ports,
}

type DispatchFn = fn(Opcode, &mut Emu, &mut Ctx) -> bool;

type IsaDef = (&'static [u8],
               fn(Opcode) -> InsnInfo,
               DispatchFn);
static ISA_DEFS: &[IsaDef] = &[
    (b"01110110",
     |_| InsnInfo::inherent("HLT"),
     |_, st, _| {
         st.advance(4);
         true
     }
    ),
    (b"01110sss",
     |opcode| InsnInfo::binary("MOV", opcode.regm(5,3), opcode.regm(2,0)),
     |opcode, st, _| {
         let v = st.reg(opcode.bits(2,0));
         let addr = st.reg_pair(RegPair::HL);
         st.store(addr, v);
         st.advance(7)
     }
    ),
    (b"01ddd110",
     |opcode| InsnInfo::binary("MOV", opcode.regm(5,3), opcode.regm(2,0)),
     |opcode, st, _| {
         let addr = st.reg_pair(RegPair::HL);
         let v = st.load(addr);
         st.set_reg(opcode.bits(5,3), v);
         st.advance(7)
     }
    ),
    (b"01dddsss",
     |opcode| InsnInfo::binary("MOV", opcode.regm(5,3), opcode.regm(2,0)),
     |opcode, st, _| {
         let v = st.reg(opcode.bits(2,0));
         st.set_reg(opcode.bits(5,3), v);
         st.advance(5)
     }
    ),
    (b"00ddd110",
     |opcode| InsnInfo::binary("MVI", opcode.regm(5,3), Operand::I8),
     |opcode, st, _| {
         let v = st.take_imm8();
         st.set_reg_m(opcode.bits(5,3), v);
         st.advance(10)
     }
    ),
    (b"00pp0001",
     |opcode| InsnInfo::binary("LXI", opcode.rp(5,4), Operand::I16),
     |opcode, st, _| {
         let v = st.take_imm16();
         st.set_reg_pair(opcode.bits(5,4), v);
         st.advance(10)
     }
    ),
    (b"00101010",
     |_| InsnInfo::unary("LHLD", Operand::Addr),
     |_, st, _| {
         let addr = st.take_imm16();
         let v = st.load16(addr);
         st.set_reg_pair(RegPair::HL, v);
         st.advance(16)
     }
    ),
    (b"00100010",
     |_| InsnInfo::unary("SHLD", Operand::Addr),
     |_, st, _| {
         let addr = st.take_imm16();
         let v = st.reg_pair(RegPair::HL);
         st.store16(addr, v);
         st.advance(16)
     }
    ),
    (b"00111010",
     |_| InsnInfo::unary("LDA", Operand::Addr),
     |_, st, _| {
         let addr = st.take_imm16();
         let v = st.load(addr);
         st.set_reg(Reg::A, v);
         st.advance(13)
     }
    ),
    (b"00pp1010",
     |opcode| InsnInfo::unary("LDAX", opcode.rp(5,4)),
     |opcode, st, _| {
         let addr = st.reg_pair(opcode.bits(5,4));
         let v = st.load(addr);
         st.set_reg(Reg::A, v);
         st.advance(7)
     }
    ),
    (b"00110010",
     |_| InsnInfo::unary("STA", Operand::Addr),
     |_, st, _| {
         let addr = st.take_imm16();
         let v = st.reg(Reg::A);
         st.store(addr, v);
         st.advance(13)
     }
    ),
    (b"00pp0010",
     |opcode| InsnInfo::unary("STAX", opcode.rp(5,4)),
     |opcode, st, _| {
         let addr = st.reg_pair(opcode.bits(5,4));
         let v = st.reg(Reg::A);
         st.store(addr, v);
         st.advance(7)
     }
    ),
    (b"11101011",
     |_| InsnInfo::inherent("XCHG"),
     |_, st, _| {
         let de = st.reg_pair(RegPair::DE);
         let hl = st.reg_pair(RegPair::HL);
         st.set_reg_pair(RegPair::DE, hl);
         st.set_reg_pair(RegPair::HL, de);
         st.advance(4)
     }
    ),
    (b"11nnn111",
     |opcode| InsnInfo::unary("RST", opcode.con(5,3)),
     |opcode, st, _| {
         let addr = opcode.bits::<u16>(5,3) << 3;
         st.call(addr);
         st.advance(11)
     }
    ),
    (b"11__1101",
     |_| InsnInfo::unary("CALL", Operand::Addr),
     |_, st, _| {
         let addr = st.take_imm16();
         st.call(addr);
         st.advance(17)
     }
    ),
    (b"11ccc100",
     |opcode| InsnInfo::unary(("C", opcode.bits(5,3)), Operand::Addr),
     |opcode, st, _| {
         let addr = st.take_imm16();
         if st.flags.condition(opcode.bits(5,3)) {
             st.call(addr);
             st.advance(17)
         } else {
             st.advance(11)
         }
     }
    ),
    (b"110_1001",
     |_| InsnInfo::inherent("RET"),
     |_, st, _| {
         st.ret();
         st.advance(10)
     }
    ),
    (b"11ccc000",
     |opcode| InsnInfo::inherent(("R", opcode.bits(5,3))),
     |opcode, st, _| {
         if st.flags.condition(opcode.bits(5,3)) {
             st.ret();
             st.advance(11)
         } else {
             st.advance(5)
         }
     }
    ),
    (b"11110101",
     |_| InsnInfo::unary("PUSH", Operand::PSW),
     |_, st, _| {
         let psw = ((st.reg(Reg::A) as u16) << 8) | st.flags.bits();
         st.push(psw);
         st.advance(11)
     }
    ),
    (b"11pp0101",
     |opcode| InsnInfo::unary("PUSH", opcode.rp(5,4)),
     |opcode, st, _| {
         let v = st.reg_pair(opcode.bits(5,4));
         st.push(v);
         st.advance(11)
     }
    ),

    (b"11110001",
     |_| InsnInfo::unary("POP", Operand::PSW),
     |_, st, _| {
         let psw = st.pop();
         st.set_reg(Reg::A, (psw >> 8) as u8);
         st.flags.from_psw(psw);
         st.advance(10)
     }
    ),
    (b"11pp0001",
     |opcode| InsnInfo::unary("POP", opcode.rp(5,4)),
     |opcode, st, _| {
         let v = st.pop();
         st.set_reg_pair(opcode.bits(5,4), v);
         st.advance(10)
     }
    ),
    (b"1100_011",
     |_| InsnInfo::unary("JMP", Operand::Addr),
     |_, st, _| {
         let addr = st.take_imm16();
         st.jump(addr);
         st.advance(10)
     }
    ),
    (b"11ccc010",
     |opcode| InsnInfo::unary(("J", opcode.bits(5,3)), Operand::Addr),
     |opcode, st, _| {
         let addr = st.take_imm16();
         if st.flags.condition(opcode.bits(5,3)) {
             st.jump(addr)
         }
         st.advance(10)
     }
    ),

    (b"00pp1001",
     |opcode| InsnInfo::unary("DAD", opcode.rp(5,4)),
     |opcode, st, _| {
         let rp = opcode.bits(5,4);
         // This can't overflow because of zero extension.
         let r32 = st.reg_pair(rp) as u32 + st.reg_pair(RegPair::HL) as u32;
         st.set_reg_pair(RegPair::HL, r32 as u16);
         
         st.flags.carry = r32 & 0x10000 != 0;
         st.advance(10)
     },
    ),
    (b"1000csss",
     |opcode| match opcode.bit(3) {
         false => InsnInfo::unary("ADD", opcode.regm(2,0)),
         _     => InsnInfo::unary("ADC", opcode.regm(2,0)),
     },
     |opcode, st, _| addsub_template_r(opcode, st, add),
    ),
    (b"1001csss",
     |opcode| match opcode.bit(3) {
         false => InsnInfo::unary("SUB", opcode.regm(2,0)),
         _     => InsnInfo::unary("SBB", opcode.regm(2,0)),
     },
     |opcode, st, _| addsub_template_r(opcode, st, sub),
    ),

    (b"1100c110",
     |opcode| match opcode.bit(3) {
         false => InsnInfo::unary("ADI", Operand::I8),
         _     => InsnInfo::unary("ACI", Operand::I8),
     },
     |opcode, st, _| addsub_template_i(opcode, st, add),
    ),
    (b"1101c110",
     |opcode| match opcode.bit(3) {
         false => InsnInfo::unary("SUI", Operand::I8),
         _     => InsnInfo::unary("SBI", Operand::I8),
     },
     |opcode, st, _| addsub_template_i(opcode, st, sub),
    ),
    (b"10100sss",
     |opcode| InsnInfo::unary("ANA", opcode.regm(2,0)),
     |opcode, st, _| logic_template_r(opcode, st, and),
    ),
    (b"11100110",
     |_| InsnInfo::unary("ANI", Operand::I8),
     |_, st, _| logic_template_i(st, and),
    ),
    (b"10110sss",
     |opcode| InsnInfo::unary("ORA", opcode.regm(2,0)),
     |opcode, st, _| logic_template_r(opcode, st, or),
    ),
    (b"11110110",
     |_| InsnInfo::unary("ORI", Operand::I8),
     |_, st, _| logic_template_i(st, or),
    ),
    (b"10101sss",
     |opcode| InsnInfo::unary("XRA", opcode.regm(2,0)),
     |opcode, st, _| logic_template_r(opcode, st, xor),
    ),
    (b"11101110",
     |_| InsnInfo::unary("XRI", Operand::I8),
     |_, st, _| logic_template_i(st, xor),
    ),
    (b"10111sss",
     |opcode| InsnInfo::unary("CMP", opcode.regm(2,0)),
     |opcode, st, _| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         compare(a, b, &mut st.flags);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11111110",
     |_| InsnInfo::unary("CPI", Operand::I8),
     |_, st, _| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         compare(a, b, &mut st.flags);
         st.advance(7)
     },
    ),
    (b"00ddd100",
     |opcode| InsnInfo::unary("INR", opcode.regm(5,3)),
     |opcode, st, _| {
        let rm = opcode.bits(5,3);
        let a = st.reg_m(rm);
        let r = inc(a, &mut st.flags);
        st.set_reg_m(rm, r);
        st.advance(rm.reg_or_m(5, 10))
     },
    ),
    (b"00ddd101",
     |opcode| InsnInfo::unary("DCR", opcode.regm(5,3)),
     |opcode, st, _| {
        let rm = opcode.bits(5,3);
        let a = st.reg_m(rm);
        let r = dec(a, &mut st.flags);
        st.set_reg_m(rm, r);
        st.advance(rm.reg_or_m(5, 10))
     },
    ),
    (b"00pp0011",
     |opcode| InsnInfo::unary("INX", opcode.rp(5,4)),
     |opcode, st, _| {
        let rp = opcode.bits(5,4);
        let a = st.reg_pair(rp);
        st.set_reg_pair(rp, a.wrapping_add(1));
        st.advance(5)
     },
    ),
    (b"00pp1011",
     |opcode| InsnInfo::unary("DCX", opcode.rp(5,4)),
     |opcode, st, _| {
        let rp = opcode.bits(5,4);
        let a = st.reg_pair(rp);
        st.set_reg_pair(rp, a.wrapping_sub(1));
        st.advance(5)
     },
    ),
    (b"00110111",
     |_| InsnInfo::inherent("STC"),
     |_, st, _| {
         st.flags.carry = true;
         st.advance(4)
     },
    ),
    (b"00101111",
     |_| InsnInfo::inherent("CMA"),
     |_, st, _| {
         let a = st.reg(Reg::A);
         st.set_reg(Reg::A, !a);
         st.advance(4)
     },
    ),
    (b"00111111",
     |_| InsnInfo::inherent("CMC"),
     |_, st, _| {
         let c = st.flags.carry;
         st.flags.carry = !c;
         st.advance(4)
     },
    ),
    (b"00000111",
     |_| InsnInfo::inherent("RLC"),
     |_, st, _| {
         let a = st.reg(Reg::A);
         let r = (a << 1) | (a >> 7);

         st.flags.carry = a & 0x80 != 0;

         st.set_reg(Reg::A, r);
         st.advance(4)
     },
    ),
    (b"00010111",
     |_| InsnInfo::inherent("RAL"),
     |_, st, _| {
         let a = st.reg(Reg::A);
         // Carry goes into low bit,
         let r = (a << 1) | (st.flags.carry as u8);
         st.set_reg(Reg::A, r);
         // and high bit goes into carry.
         st.flags.carry = a & 0x80 != 0;

         st.advance(4)
     },
    ),
    (b"00001111",
     |_| InsnInfo::inherent("RRC"),
     |_, st, _| {
         let a = st.reg(Reg::A);
         let r = (a >> 1) | (a << 7);

         st.flags.carry = a & 1 != 0;

         st.set_reg(Reg::A, r);
         st.advance(4)
     },
    ),
    (b"00011111",
     |_| InsnInfo::inherent("RAR"),
     |_, st, _| {
         let a = st.reg(Reg::A);
         // Carry goes into high bit,
         let r = (a >> 1) | (st.flags.carry as u8) << 7;
         st.set_reg(Reg::A, r);
         // And low bit goes into carry.
         st.flags.carry = a & 1 != 0;

         st.advance(4)
     },
    ),
    (b"00100111",
     |_| InsnInfo::inherent("DAA"),
     |_, st, _| {
         // back up carry flag.
         let mut carry = st.flags.carry;
         let a = st.reg(Reg::A);
         let mut b = 0;
         if st.flags.aux || (a & 0xF) > 9 {
             b = 6;
         }
         if st.flags.carry ||
             (a >> 4) > 9 ||
             ((a >> 4) == 9 && (a & 0xF) > 9) {
            b |= 0x60;
            carry |= true;
         }
         let r = add(a, b, 0, &mut st.flags);
         st.set_reg(Reg::A, r);
         st.flags.carry |= carry;
         st.advance(4)
     },
    ),
    (b"11100011",
     |_| InsnInfo::inherent("XTHL"),
     |_, st, _| {
         let hl = st.reg_pair(RegPair::HL);
         let tmp = st.pop();
         st.push(hl);
         st.set_reg_pair(RegPair::HL, tmp);
         st.advance(18)
     },
    ),
    (b"11111001",
     |_| InsnInfo::inherent("SPHL"),
     |_, st, _| {
         let hl = st.reg_pair(RegPair::HL);
         st.set_reg_pair(RegPair::SP, hl);
         st.advance(5)
     },
    ),
    (b"11101001",
     |_| InsnInfo::inherent("PCHL"),
     |_, st, _| {
         let hl = st.reg_pair(RegPair::HL);
         st.jump(hl);
         st.advance(5)
     },
    ),
    (b"11111011",
     |_| InsnInfo::inherent("EI"),
     |_, st, _| {
         st.set_interrupt_flag(true);
         st.advance(4)
     },
    ),
    (b"11110011",
     |_| InsnInfo::inherent("DI"),
     |_, st, _| {
         st.set_interrupt_flag(false);
         st.advance(4)
     },
    ),
    (b"11011011",
     |_| InsnInfo::unary("IN", Operand::Port8),
     |_, st, ctx| {
         let p = st.take_imm8();
         let v = ctx.io.read_port(p);
         st.set_reg(Reg::A, v);
         st.advance(10)
     },
    ),
    (b"11010011",
     |_| InsnInfo::unary("OUT", Operand::Port8),
     |_, st, ctx| {
         let p = st.take_imm8();
         let v = st.reg(Reg::A);
         ctx.io.write_port(p, v);
         st.advance(10)
     },
    ),
    (b"00___000",
     |_| InsnInfo::inherent("NOP"),
     |_, st, _| {
         st.advance(4)
     },
    ),

];

////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct Pat(u8, u8);

impl Pat {
    fn parse(text: &'static [u8]) -> Pat {
        debug_assert!(text.len() == 8, "Pattern has wrong length: {:?}", text);

        let mut mask = 0;
        let mut bits = 0;
        for i in 0..8 {
            if text[i] == b'0' || text[i] == b'1' {
                mask |= 1 << (7 - i);
            }
            if text[i] == b'1' {
                bits |= 1 << (7 - i);
            }
        }

        Pat(mask, bits)
    }

    fn matches(&self, bits: u8) -> bool {
        bits & self.0 == self.1
    }
}

/// Order patterns by specificity, from most 1s in mask to fewest.
impl ::std::cmp::Ord for Pat {
    fn cmp(&self, other: &Pat) -> ::std::cmp::Ordering {
        other.0.count_ones().cmp(&self.0.count_ones())
            .then(self.0.cmp(&other.0))
            .then(self.1.cmp(&other.1))
    }
}

impl ::std::cmp::PartialOrd for Pat {
    fn partial_cmp(&self, other: &Pat) -> Option<::std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

lazy_static! {
    pub static ref DISPATCH: [Option<DispatchFn>; 256] = {
        let mut table = [None; 256];

        // Collect all the dispatch functions and sort them by pattern
        // specificity.
        let defs = {
            let mut defs: Vec<_> = ISA_DEFS.iter()
                .map(|(pat, _, op)| (Pat::parse(pat), op))
                .collect();
            defs.sort_unstable_by_key(|&(p, _)| p);
            defs
        };

        // Identify the pattern that applies to each possible 8-bit instruction
        // and register its dispatch function.
        for opc in 0..=255 {
            for &(pat, &op) in &defs {
                if pat.matches(opc) {
                    table[opc as usize] = Some(op);
                    // Because we've sorted from most specific to least, the
                    // first matching pattern is the right one. Skip the rest.
                    break;
                }
            }
        }

        table
    };
}

pub fn make_decode_table() -> Vec<Option<fn(Opcode) -> InsnInfo>> {
    let mut defs: Vec<_> = ISA_DEFS.iter()
        .map(|(pat, dec, _)| (Pat::parse(pat), dec))
        .collect();
    defs.sort_unstable_by_key(|&(p, _)| p);
    let defs = defs;

    let mut table = Vec::new();

    for opc in 0..=255 {
        table.push(None);

        for &(pat, &op) in &defs {
            if pat.matches(opc) {
                *table.last_mut().unwrap() = Some(op);
                break;
            }
        }
    }

    table
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn check_isa_def_uniqueness() {
        let mut seen: HashMap<(u8, u8), Vec<_>> = HashMap::new();

        for (pat_str, dec, _) in ISA_DEFS.iter() {
            let pat = Pat::parse(pat_str);
            seen.entry((pat.0, pat.1))
                .and_modify(|v| v.push((pat_str, dec)))
                .or_insert_with(|| vec![(pat_str, dec)]);
        }

        let mut dups: Vec<Vec<_>> = Vec::new();
        for (key, pats) in seen {
            if pats.len() > 1 {
                dups.push(pats.into_iter().map(|(p, dec)| {
                    let insn = dec(Opcode(key.1));
                    let txt = String::from_utf8_lossy(p);
                    (txt, insn)
                }).collect());
            }
        }

        if !dups.is_empty() {
            panic!("Duplicate pattern groups: {:#?}", dups)
        }
    }
}

