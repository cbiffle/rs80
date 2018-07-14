/// A simple emulator for the 8080 (or, really, a clone thereof -- this is
/// tested against an emulator for the Soviet KR580VM80A).

extern crate time;
extern crate rs80;

use rs80::*;

use std::io;
use time::PreciseTime;

fn main() -> std::io::Result<()> {
    {
        let table = make_decode_table();
        let mut covered = 0;
        for i in 0..=255 {
            if table[i].is_some() {
                covered += 1;
            }
        }
        println!("{} / 256 encodings covered.", covered);
    }

    let mut args = std::env::args();
    args.next();

    let filename = match args.next() {
        None => panic!("missing image filename"),
        Some(f) => f,
    };

    let mut emu = Emu::default();
    load_image(filename, &mut emu)?;

    let start = PreciseTime::now();
    let out = io::stdout();
    let mut out = out.lock();
    match run(&mut emu, &mut out) {
        Ok(final_pc) => println!("\nWARM BOOT from {:04X}", final_pc),
        Err(RunError::UnhandledBdosCall(c, pc)) =>
            println!("\nERROR: unhandled BDOS call {} at {:04X}", c, pc),
        Err(RunError::BadJump(dest, src)) =>
            println!("\nERROR: bad jump to {:04X} from {:04X}", dest, src),
        Err(RunError::UnimplementedInstruction(op, pc)) =>
            println!("\nERROR: unimplemented: {:02X} at {:04X}", op, pc),
        Err(RunError::Halted(pc)) =>
            println!("\nHALTED at {:04X}", pc),
        Err(RunError::Out(e)) => return Err(e),
    }
    let duration = start.to(PreciseTime::now());
    let cycle_ns = duration.num_nanoseconds().unwrap() as f64
                 / emu.cycles as f64;
    println!("Took: {} ({} cycles, {:.4} ns/cyc, {:.3} emulated MHz)",
                duration,
                emu.cycles,
                cycle_ns,
                1000. / cycle_ns);
    Ok(())
}
