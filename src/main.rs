//! A simple emulator for the 8080 (or, really, a clone thereof -- this is
//! tested against an emulator for the Soviet KR580VM80A).

use rs80::emu::{Emu, RunError};
use rs80::bdos::*;

use std::io;
use std::time::Instant;

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    args.next();

    let filename = match args.next() {
        None => panic!("missing image filename"),
        Some(f) => f,
    };

    let mut emu = Emu::default();
    load_image(filename, &mut emu)?;

    initialize_page_zero(&mut emu);

    let out = io::stdout();
    let mut out = out.lock();

    let start = Instant::now();
    match run_bdos(&mut emu, &mut (), &mut out) {
        Ok(final_pc) => println!("\nWARM BOOT from {:04X}", final_pc),
        Err(BdosError::UnhandledBdosCall(c, pc)) =>
            println!("\nERROR: unhandled BDOS call {} at {:04X}", c, pc),
        Err(BdosError::Halted(pc)) =>
            println!("\nHALTED at {:04X}", pc),
        Err(BdosError::Out(e)) => return Err(e),
        Err(BdosError::RunError(RunError::UnimplementedInstruction(op, pc))) =>
            println!("\nERROR: unimplemented: {:02X} at {:04X}", op, pc),
    }
    let duration = start.elapsed();
    println!("Took: {:?}", duration);

    #[cfg(feature = "count-cycles")]
    {
        let cycle_ns = duration.as_nanos() as f64
            / emu.cycles as f64;
        println!("8080 cycles: {}, {:.4} ns/cyc, {:.3} emulated MHz",
            emu.cycles,
            cycle_ns,
            1000. / cycle_ns);
    }

    let inst_ns = duration.as_nanos() as f64
                 / emu.inst_count as f64;
    println!("Instructions: {}, {:.4} ns/inst, {:.3} Minst/s",
                emu.inst_count,
                inst_ns,
                1000. / inst_ns);

    Ok(())
}
