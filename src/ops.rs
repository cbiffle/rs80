
use rs80_common::isa::{Opcode, Reg, RegPair, RegM, CC};
use super::emu::{Emu, Flags, Ports};

////////////////////////////////////////////////////////////////////////////////
// Reusable (or at least *reused*) arithmetic operations.

#[inline]
fn add(a: u8, b: u8, c: u8, flags: &mut Flags) -> u8 {
    // This addition can't overflow because of the zero-extension.
    let sum16 = a as u16 + b as u16 + c as u16;
   
    let sum8 = sum16 as u8; // may get truncated here though, and that's OK
    flags.zero = sum8 == 0;
    flags.carry = sum16 & 0x100 != 0;
    flags.sign = sum8 & 0x80 != 0;
    flags.aux_input = (a, b, c, false);
    flags.parity_input = sum8;

    sum8
}

#[inline]
fn inc(a: u8, flags: &mut Flags) -> u8 {
    let sum = a.wrapping_add(1);
   
    flags.zero   = sum == 0;
    flags.aux_input = (a, 1, 0, false);
    flags.parity_input = sum;
    flags.sign   = sum & 0x80 != 0;

    sum
}

#[inline]
fn dec(a: u8, flags: &mut Flags) -> u8 {
    let sum = a.wrapping_sub(1);
   
    flags.zero   = sum == 0;
    flags.aux_input = (a, 0xFF, 0, false);
    flags.parity_input = sum;
    flags.sign   = sum & 0x80 != 0;

    sum
}

#[inline]
fn and(a: u8, b: u8, flags: &mut Flags) -> u8 {
    let r = a & b;
   
    flags.zero   = r == 0;
    flags.aux_input = (0,0,0, (a & 0x8) | (b & 0x8) != 0);
    flags.sign   = r & 0x80 != 0;
    flags.parity_input = r;
    flags.carry  = false;

    r
}

#[inline]
fn or(a: u8, b: u8, flags: &mut Flags) -> u8 {
    let r = a | b;
   
    flags.zero = r == 0;
    flags.carry = false;
    flags.aux_input = (0, 0, 0, false);
    flags.sign = r & 0x80 != 0;
    flags.parity_input = r;

    r
}

#[inline]
fn xor(a: u8, b: u8, flags: &mut Flags) -> u8 {
    let r = a ^ b;
   
    flags.zero = r == 0;
    flags.carry = false;
    flags.aux_input = (0, 0, 0, false);
    flags.sign = r & 0x80 != 0;
    flags.parity_input = r;

    r
}

#[inline]
fn sub(a: u8, b: u8, c: u8, flags: &mut Flags) -> u8 {
    // We'll do this subtraction at 16 bit width to get access to carry.
    let r16 = (a as u16).wrapping_sub(b as u16).wrapping_sub(c as u16);
   
    let r = r16 as u8;
    flags.zero   = r == 0;
    flags.carry  = r16 & 0x100 != 0;
    flags.sign   = r & 0x80 != 0;
    flags.aux_input = (a, !b, 1 - c, false);
    flags.parity_input = r;

    r
}

////////////////////////////////////////////////////////////////////////////////
// Instruction implementation templates. These are parameterized on the function
// passed in (note the `impl`) so they get specialized and optimized.

/// Non-immediate add or subtract.
#[inline]
fn addsub_template_r2(opcode: Opcode,
                      rm: RegM,
                      st: &mut Emu,
                      f: impl FnOnce(u8, u8, u8, &mut Flags) -> u8) {
    let a = st.reg(Reg::A);
    let b = st.reg_m(rm);
    let c = match opcode.bit(3) {
        false => 0,
        _     => st.flags.carry as u8,
    };
    let a = f(a, b, c, &mut st.flags);
    st.set_reg(Reg::A, a);
}

/// Immediate add or subtract.
#[inline]
fn addsub_template_i2(opcode: Opcode,
                      b: u8,
                      st: &mut Emu,
                      f: impl FnOnce(u8, u8, u8, &mut Flags) -> u8) {
    let a = st.reg(Reg::A);
    let c = match opcode.bit(3) {
        false => 0,
        _     => st.flags.carry as u8,
    };
    let a = f(a, b, c, &mut st.flags);
    st.set_reg(Reg::A, a);
}

/// Non-immediate logic.
#[inline]
fn logic_template_r2(rm: RegM,
                     st: &mut Emu,
                     f: impl FnOnce(u8, u8, &mut Flags) -> u8) {
    let a = st.reg(Reg::A);
    let b = st.reg_m(rm);
    let a = f(a, b, &mut st.flags);
    st.set_reg(Reg::A, a);
}

/// Immediate logic.
#[inline]
fn logic_template_i2(st: &mut Emu,
                     b: u8,
                     f: impl FnOnce(u8, u8, &mut Flags) -> u8) {
    let a = st.reg(Reg::A);
    let a = f(a, b, &mut st.flags);
    st.set_reg(Reg::A, a);
}

/// Compare is just subtract with the result discarded.
fn compare(a: u8, b: u8, flags: &mut Flags) {
    sub(a, b, 0, flags);
}

pub struct Ctx<'a> {
    pub io: &'a mut Ports,
}

include!(concat!(env!("OUT_DIR"), "/dispatch.rs"));
