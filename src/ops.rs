use std::num::Wrapping;

use super::isa::{Insn, Opcode, Reg, RegPair};
use super::emu::{Emu, Flags, W8};

fn u16_from_slice(slice: &[u8]) -> u16 {
    (slice[0] as u16) | ((slice[1] as u16) << 8)
}

////////////////////////////////////////////////////////////////////////////////

fn add(a: W8, b: W8, c: u8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 + b.0 as u16 + c as u16;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.carry = sum16 & 0x100 != 0;
    flags.sign = sum8 & 0x80 != 0;
    flags.aux = ((a.0 & 0xF) + (b.0 & 0xF) + c) & 0x10 != 0;
    flags.parity = sum8.count_ones() % 2 == 0;

    Wrapping(sum8)
}

fn inc(a: W8, flags: &mut Flags) -> W8 {
    let sum = a + Wrapping(1);
   
    flags.zero   = sum.0 == 0;
    flags.aux    = (sum.0 & 0xF) == 0;
    flags.parity = sum.0.count_ones() % 2 == 0;
    flags.sign   = sum.0 & 0x80 != 0;

    sum
}

fn dec(a: W8, flags: &mut Flags) -> W8 {
    let sum = a - Wrapping(1);
   
    flags.zero   = sum.0 == 0;
    flags.aux    = (sum.0 & 0xF) != 0xF;
    flags.parity = sum.0.count_ones() % 2 == 0;
    flags.sign   = sum.0 & 0x80 != 0;

    sum
}

fn and(a: W8, b: W8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 & b.0 as u16;
   
    let sum8 = sum16 as u8;
    flags.zero   = sum8 == 0;
    flags.aux    = (a.0 & 0x8) | (b.0 & 0x8) != 0;
    flags.sign   = sum8 & 0x80 != 0;
    flags.parity = sum8.count_ones() % 2 == 0;
    flags.carry = false;

    Wrapping(sum8)
}

fn or(a: W8, b: W8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 | b.0 as u16;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.carry = false;
    flags.aux = false;
    flags.sign = sum8 & 0x80 != 0;
    flags.parity = sum8.count_ones() % 2 == 0;

    Wrapping(sum8)
}

fn xor(a: W8, b: W8, flags: &mut Flags) -> W8 {
    let sum16 = a.0 as u16 ^ b.0 as u16;
   
    let sum8 = sum16 as u8;
    flags.zero = sum8 == 0;
    flags.carry = false;
    flags.aux = false;
    flags.sign = sum8 & 0x80 != 0;
    flags.parity = sum8.count_ones() % 2 == 0;

    Wrapping(sum8)
}

fn sub(a: W8, b: W8, c: u8, flags: &mut Flags) -> W8 {
    let sum16 = (Wrapping(a.0 as u16) - Wrapping(b.0 as u16) - Wrapping(c as u16)).0;
   
    let sum8 = sum16 as u8;
    flags.zero   = sum8 == 0;
    flags.carry  = sum16 & 0x100 != 0;
    flags.sign   = sum8 & 0x80 != 0;
    flags.aux    = ((a.0 & 0xF) + (!b.0 & 0xF) + (1 - c)) & 0x10 != 0;
    flags.parity = sum8.count_ones() % 2 == 0;

    Wrapping(sum8)
}

fn compare(a: W8, b: W8, flags: &mut Flags) {
    sub(a, b, 0, flags);
}

type IsaDef = (&'static [u8],
               fn(Opcode, &[u8]) -> Insn,
               fn(Opcode, &mut Emu));
static ISA_DEFS: &[IsaDef] = &[
    (b"01110110",
     |_, _| Insn::Hlt,
     |_, st| {
         st.halt();
         st.advance(4);  // Seriously.
     }
    ),
    (b"01110sss",
     |opcode, _| Insn::MovToM(opcode.bits(2,0)),
     |opcode, st| {
         let v = st.reg(opcode.bits(2,0));
         let addr = st.reg_pair(RegPair::HL);
         st.store(addr, v);
         st.advance(7)
     }
    ),
    (b"01ddd110",
     |opcode, _| Insn::MovFromM(opcode.bits(5,3)),
     |opcode, st| {
         let addr = st.reg_pair(RegPair::HL);
         let v = st.load(addr);
         st.set_reg(opcode.bits(5,3), v);
         st.advance(7)
     }
    ),
    (b"01dddsss",
     |opcode, _| Insn::Mov(opcode.bits(5,3), opcode.bits(2,0)),
     |opcode, st| {
         let v = st.reg(opcode.bits(2,0));
         st.set_reg(opcode.bits(5,3), v);
         st.advance(5)
     }
    ),
    (b"00110110",
     |_, b| Insn::MviToM(b[0]),
     |_, st| {
         let v = st.take_imm8();
         let addr = st.reg_pair(RegPair::HL);
         st.store(addr, v);
         st.advance(10)
     }
    ),
    (b"00ddd110",
     |opcode, b| Insn::Mvi(opcode.bits(5,3), b[0]),
     |opcode, st| {
         let v = st.take_imm8();
         st.set_reg(opcode.bits(5,3), v);
         st.advance(10)
     }
    ),
    (b"00pp0001",
     |opcode, b| Insn::Lxi(opcode.bits(5,4), u16_from_slice(b)),
     |opcode, st| {
         let v = st.take_imm16();
         st.set_reg_pair(opcode.bits(5,4), v);
         st.advance(10)
     }
    ),
    (b"00101010",
     |_, b| Insn::Lhld(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.load16(addr);
         st.set_reg_pair(RegPair::HL, v);
         st.advance(16)
     }
    ),
    (b"00100010",
     |_, b| Insn::Shld(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.reg_pair(RegPair::HL);
         st.store16(addr, v);
         st.advance(16)
     }
    ),
    (b"00111010",
     |_, b| Insn::Lda(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.load(addr);
         st.set_reg(Reg::A, v);
         st.advance(13)
     }
    ),
    (b"00pp1010",
     |opcode, _| Insn::Ldax(opcode.bits(5,4)),
     |opcode, st| {
         let addr = st.reg_pair(opcode.bits(5,4));
         let v = st.load(addr);
         st.set_reg(Reg::A, v);
         st.advance(7)
     }
    ),
    (b"00110010",
     |_, b| Insn::Sta(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         let v = st.reg(Reg::A);
         st.store(addr, v);
         st.advance(13)
     }
    ),
    (b"00pp0010",
     |opcode, _| Insn::Stax(opcode.bits(5,4)),
     |opcode, st| {
         let addr = st.reg_pair(opcode.bits(5,4));
         let v = st.reg(Reg::A);
         st.store(addr, v);
         st.advance(7)
     }
    ),
    (b"11101011",
     |_, _| Insn::Xchg,
     |_, st| {
         let de = st.reg_pair(RegPair::DE);
         let hl = st.reg_pair(RegPair::HL);
         st.set_reg_pair(RegPair::DE, hl);
         st.set_reg_pair(RegPair::HL, de);
         st.advance(4)
     }
    ),
    (b"11nnn111",
     |opcode, _| Insn::Rst(opcode.bits(5,3)),
     |opcode, st| {
         let addr = Wrapping(opcode.bits::<u16>(5,3) * 8);
         st.call(addr);
         st.advance(11)
     }
    ),
    (b"11001101",
     |_, b| Insn::Call(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         st.call(addr);
         st.advance(17)
     }
    ),
    (b"11ccc100",
     |opcode, b| Insn::C(opcode.bits(5,3), u16_from_slice(b)),
     |opcode, st| {
         let addr = st.take_imm16();
         if st.flags.condition(opcode.bits(5,3)) {
             st.call(addr);
             st.advance(17)
         } else {
             st.advance(11)
         }
     }
    ),
    (b"11001001",
     |_, _| Insn::Ret,
     |_, st| {
         st.ret();
         st.advance(10)
     }
    ),
    (b"11ccc000",
     |opcode, _| Insn::R(opcode.bits(5,3)),
     |opcode, st| {
         if st.flags.condition(opcode.bits(5,3)) {
             st.ret();
             st.advance(11)
         } else {
             st.advance(5)
         }
     }
    ),
    (b"11110101",
     |_, _| Insn::PushPSW,
     |_, st| {
         let psw = ((st.reg(Reg::A).0 as u16) << 8)
             | (st.flags.bits() as u16);
         st.push(Wrapping(psw));
         st.advance(11)
     }
    ),
    (b"11pp0101",
     |opcode, _| Insn::Push(opcode.bits(5,4)),
     |opcode, st| {
         let v = st.reg_pair(opcode.bits(5,4));
         st.push(v);
         st.advance(11)
     }
    ),

    (b"11110001",
     |_, _| Insn::PopPSW,
     |_, st| {
         let psw = st.pop().0;
         st.set_reg(Reg::A, Wrapping((psw >> 8) as u8));
         st.flags.from_bits(psw as u8);
         st.advance(10)
     }
    ),
    (b"11pp0001",
     |opcode, _| Insn::Pop(opcode.bits(5,4)),
     |opcode, st| {
         let v = st.pop();
         st.set_reg_pair(opcode.bits(5,4), v);
         st.advance(10)
     }
    ),
    (b"11000011",
     |_, b| Insn::Jmp(u16_from_slice(b)),
     |_, st| {
         let addr = st.take_imm16();
         st.jump(addr);
         st.advance(10)
     }
    ),
    (b"11ccc010",
     |opcode, b| Insn::J(opcode.bits(5,3), u16_from_slice(b)),
     |opcode, st| {
         let addr = st.take_imm16();
         if st.flags.condition(opcode.bits(5,3)) {
             st.jump(addr)
         }
         st.advance(10)
     }
    ),

    (b"00pp1001",
     |opcode, _| Insn::Dad(opcode.bits(5,4)),
     |opcode, st| {
         let rp = opcode.bits(5,4);
         let a = st.reg_pair(rp);
         let r32 = a.0 as u32 + st.reg_pair(RegPair::HL).0 as u32;
         st.set_reg_pair(RegPair::HL, Wrapping(r32 as u16));
         
         st.flags.carry = r32 & 0x10000 != 0;
         st.advance(10)
     },
    ),
    (b"10000sss",
     |opcode, _| Insn::Add(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         let a = add(a, b, 0, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(rm.reg_or_m(4,7))
     },
    ),
    (b"10001sss",
     |opcode, _| Insn::Adc(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         let c = st.flags.carry as u8;
         let a = add(a, b, c, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"10010sss",
     |opcode, _| Insn::Sub(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         let a = sub(a, b, 0, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"10011sss",
     |opcode, _| Insn::Sbb(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         let c = st.flags.carry as u8;
         let a = sub(a, b, c, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11000110",
     |_, b| Insn::Adi(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         let a = add(a, b, 0, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(7)
     },
    ),
    (b"11001110",
     |_, b| Insn::Aci(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         let c = st.flags.carry as u8;
         let a = add(a, b, c, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(7)
     },
    ),
    (b"11010110",
     |_, b| Insn::Sui(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         let a = sub(a, b, 0, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(7)
     },
    ),
    (b"11011110",
     |_, b| Insn::Sbi(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         let c = st.flags.carry as u8;
         let a = sub(a, b, c, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(7)
     },
    ),
    (b"10100sss",
     |opcode, _| Insn::Ana(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         let a = and(a, b, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11100110",
     |_, b| Insn::Ani(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         let a = and(a, b, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(7)
     },
    ),
    (b"10110sss",
     |opcode, _| Insn::Ora(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         let a = or(a, b, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11110110",
     |_, b| Insn::Ori(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         let a = or(a, b, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(7)
     },
    ),
    (b"10101sss",
     |opcode, _| Insn::Xra(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         let a = xor(a, b, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11101110",
     |_, b| Insn::Xri(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         let a = xor(a, b, &mut st.flags);
         st.set_reg(Reg::A, a);
         st.advance(7)
     },
    ),
    (b"10111sss",
     |opcode, _| Insn::Cmp(opcode.bits(2,0)),
     |opcode, st| {
         let rm = opcode.bits(2,0);
         let a = st.reg(Reg::A);
         let b = st.reg_m(rm);
         compare(a, b, &mut st.flags);
         st.advance(rm.reg_or_m(4, 7))
     },
    ),
    (b"11111110",
     |_, b| Insn::Cpi(b[0]),
     |_, st| {
         let a = st.reg(Reg::A);
         let b = st.take_imm8();
         compare(a, b, &mut st.flags);
         st.advance(7)
     },
    ),
    (b"00ddd100",
     |opcode, _| Insn::Inr(opcode.bits(5,3)),
     |opcode, st| {
        let rm = opcode.bits(5,3);
        let a = st.reg_m(rm);
        let r = inc(a, &mut st.flags);
        st.set_reg_m(rm, r);
        st.advance(rm.reg_or_m(5, 10))
     },
    ),
    (b"00ddd101",
     |opcode, _| Insn::Dcr(opcode.bits(5,3)),
     |opcode, st| {
        let rm = opcode.bits(5,3);
        let a = st.reg_m(rm);
        let r = dec(a, &mut st.flags);
        st.set_reg_m(rm, r);
        st.advance(rm.reg_or_m(5, 10))
     },
    ),
    (b"00pp0011",
     |opcode, _| Insn::Inx(opcode.bits(5,4)),
     |opcode, st| {
        let rp = opcode.bits(5,4);
        let a = st.reg_pair(rp);
        st.set_reg_pair(rp, a + Wrapping(1));
        st.advance(5)
     },
    ),
    (b"00pp1011",
     |opcode, _| Insn::Dcx(opcode.bits(5,4)),
     |opcode, st| {
        let rp = opcode.bits(5,4);
        let a = st.reg_pair(rp);
        st.set_reg_pair(rp, a - Wrapping(1));
        st.advance(5)
     },
    ),
    (b"00110111",
     |_, _| Insn::Stc,
     |_, st| {
         st.flags.carry = true;
         st.advance(4)
     },
    ),
    (b"00101111",
     |_, _| Insn::Cma,
     |_, st| {
         let a = st.reg(Reg::A);
         st.set_reg(Reg::A, !a);
         st.advance(4)
     },
    ),
    (b"00111111",
     |_, _| Insn::Cmc,
     |_, st| {
         let c = st.flags.carry;
         st.flags.carry = !c;
         st.advance(4)
     },
    ),
    (b"00000111",
     |_, _| Insn::Rlc,
     |_, st| {
         let a = st.reg(Reg::A);
         let r = (a << 1) | (a >> 7);

         st.flags.carry = a.0 & 0x80 != 0;

         st.set_reg(Reg::A, r);
         st.advance(4)
     },
    ),
    (b"00010111",
     |_, _| Insn::Ral,
     |_, st| {
         let a = st.reg(Reg::A);
         let r = (a << 1) | Wrapping(st.flags.carry as u8);

         st.flags.carry = a.0 & 0x80 != 0;

         st.set_reg(Reg::A, r);
         st.advance(4)
     },
    ),
    (b"00001111",
     |_, _| Insn::Rrc,
     |_, st| {
         let a = st.reg(Reg::A);
         let r = (a >> 1) | (a << 7);

         st.flags.carry = a.0 & 1 != 0;

         st.set_reg(Reg::A, r);
         st.advance(4)
     },
    ),
    (b"00011111",
     |_, _| Insn::Rar,
     |_, st| {
         let a = st.reg(Reg::A);
         let r = (a >> 1) | Wrapping((st.flags.carry as u8) << 7);

         st.flags.carry = a.0 & 1 != 0;

         st.set_reg(Reg::A, r);
         st.advance(4)
     },
    ),
    (b"00100111",
     |_, _| Insn::Daa,
     |_, st| {
         // back up carry flag.
         let mut carry = st.flags.carry;
         let a = st.reg(Reg::A);
         let mut b = 0;
         if st.flags.aux || (a.0 & 0xF) > 9 {
             b = 6;
         }
         if st.flags.carry ||
             (a.0 >> 4) > 9 ||
             ((a.0 >> 4) == 9 && (a.0 & 0xF) > 9) {
            b |= 0x60;
            carry |= true;
         }
         let r = add(a, Wrapping(b), 0, &mut st.flags);
         st.set_reg(Reg::A, r);
         st.flags.carry |= carry;
         st.advance(4)
     },
    ),
    (b"11100011",
     |_, _| Insn::Xthl,
     |_, st| {
         let hl = st.reg_pair(RegPair::HL);
         let tmp = st.pop();
         st.push(hl);
         st.set_reg_pair(RegPair::HL, tmp);
         st.advance(18)
     },
    ),
    (b"11111001",
     |_, _| Insn::Sphl,
     |_, st| {
         let hl = st.reg_pair(RegPair::HL);
         st.set_reg_pair(RegPair::SP, hl);
         st.advance(5)
     },
    ),
    (b"11101001",
     |_, _| Insn::Pchl,
     |_, st| {
         let hl = st.reg_pair(RegPair::HL);
         st.jump(hl);
         st.advance(5)
     },
    ),
    (b"11111011",
     |_, _| Insn::Ei,
     |_, st| {
         st.set_interrupt_flag(true);
         st.advance(4)
     },
    ),
    (b"11110011",
     |_, _| Insn::Di,
     |_, st| {
         st.set_interrupt_flag(false);
         st.advance(4)
     },
    ),
    (b"00___000",
     |_, _| Insn::Nop,
     |_, st| {
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
    pub static ref DISPATCH: [Option<fn(Opcode, &mut Emu)>; 256] = {
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

pub fn make_decode_table() -> Vec<Option<fn(Opcode, &[u8]) -> Insn>> {
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
                    let insn = dec(Opcode(key.1), &[0, 0]);
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

