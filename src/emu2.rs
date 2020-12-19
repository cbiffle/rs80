use std::convert::TryInto;

use crate::emu;
use crate::ops;

pub struct Emu2 {
    pub core: emu::Emu,
    decodes: [(ops::OpFn, usize); 65536],
}

impl From<emu::Emu> for Emu2 {
    fn from(core: emu::Emu) -> Self {
        let decodes = ops::predecode(&core.mem[..65536]);
        Self {
            core,
            decodes: decodes.try_into().map_err(|_| ()).unwrap(),
        }
    }
}


/// Runs the emulation until the machine encounters a `HLT` (or an illegal
/// instruction).
///
/// On halt, returns `Ok(last_pc, pc)`, where `pc` is the address of the `HLT`
/// instruction, and `last_pc` is the address of the instruction executed just
/// before it. (This is often `pc-1`, but may differ if the `HLT` was reached by
/// a call or jump.)
#[inline]
pub fn run(emu: &mut Emu2, io: &mut dyn emu::Ports) -> Result<(u16, u16), emu::RunError> {
    let mut pc = 0xFFFF;
    let mut last_pc;

    let mut ctx = ops::Ctx { io };

    loop {
        // Move last instruction start into previous buffer.
        last_pc = pc;
        // Record start of this instruction.
        pc = emu.core.get_pc();
        let (run, imm) = &emu.decodes[usize::from(pc)];
        emu.core.inst_count += 1;
        let halted = run(&mut emu.core, &mut ctx, imm);
        if halted { return Ok((last_pc, pc)) }
    }
}

