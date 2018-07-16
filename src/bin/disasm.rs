extern crate rs80;
extern crate clap;

use std::io::{self, Read};
use std::fs;

use clap::{App,Arg};
use rs80::dis;

fn main() -> io::Result<()> {
    let matches = App::new("disasm")
        .version("0.1")
        .author("Cliff L. Biffle <code@cliffle.com>")
        .about("Basic 8080 disassembler")
        .arg(Arg::with_name("org")
             .long("org")
             .value_name("ADDR")
             .help("Sets assumed base address in hex (default: 0100)")
             .takes_value(true))
        .arg(Arg::with_name("INPUT")
             .help("File to read (omit for standard input)"))
        .get_matches();

    let addr = matches.value_of("org").unwrap_or("0100");
    let addr = match u16::from_str_radix(addr, 16) {
        Ok(a) => a,
        Err(_) => {
            eprintln!("Bad value for --org: {}", addr);
            std::process::exit(1)
        }
    };
    let mut addr = addr as usize;

    let stdin = io::stdin();

    let input: Box<Read> = match matches.value_of("INPUT") {
        Some(filename) => Box::new(fs::File::open(filename)?),
        None => Box::new(stdin.lock()),
    };
    let mut input = input.bytes();

    let out = io::stdout();
    let mut out = out.lock();
    loop {
        print!("{:04X}\t", addr);
        match dis::disassemble(&mut input, &mut out)? {
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

