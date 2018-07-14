extern crate rs80;

use std::io::{self, Read};
use std::env;
use std::fs;

use rs80::dis;

fn main() -> io::Result<()> {
    let mut args = env::args();
    args.next();
    let filename = match args.next() {
        None => panic!("filename argument missing"),
        Some(f) => f,
    };

    let mut file = fs::File::open(filename)?.bytes();
    let mut addr = 0x100;
    loop {
        print!("{:04X}\t", addr);
        let out = io::stdout();
        let mut out = out.lock();
        match dis::disassemble(&mut file, &mut out)? {
            0 => {
                println!("END");
                return Ok(())
            },
            n => {
                addr += n;
                print!("\n");
            },
        }
    }
}

