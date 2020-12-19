//! Build script for rs80.
//!
//! This program consumes and parses the `isa-ops.txt` that defines the
//! instruction set, and produces `dispatch.rs` implementing the execution
//! engine.

extern crate combine;
extern crate rs80_common;
extern crate rs80_gen;

use std::cmp::Reverse;
use std::io::{self, Read};
use std::fs;
use std::env;
use std::path::Path;

use combine::stream::state::State;
use combine::Parser;

use rs80_gen::Item;

fn main() -> io::Result<()> {
    println!("cargo:rerun-if-changed=isa-ops.txt");

    let mut f = fs::File::open("isa-ops.txt")?;
    let mut text = String::new();
    f.read_to_string(&mut text)?;

    let (items, _) = rs80_gen::parse::spec_file()
        .easy_parse(State::new(&*text)).unwrap();

    let out_dir = env::var("OUT_DIR").unwrap();

    // Filter out comments.
    let mut defs: Vec<_> = items.into_iter()
        .filter_map(|item| {
            if let Item::Def(def) = item {
                Some(def)
            } else {
                None
            }
        })
        .collect();

    // Process more-specific defs before less-specific defs
    defs.sort_unstable_by_key(|d| Reverse(d.bits.specificity()));

    {
        let out_path = Path::new(&out_dir).join("dispatch.rs");
        let mut out = fs::File::create(out_path)?;
        rs80_gen::gen::dispatch(&defs, &mut out)?;
    }

    {
        let out_path = Path::new(&out_dir).join("disassemble.rs");
        let mut out = fs::File::create(out_path)?;
        rs80_gen::gen::disassemble(&defs, &mut out)?;
    }

    {
        let out_path = Path::new(&out_dir).join("predecode.rs");
        let mut out = fs::File::create(out_path)?;
        rs80_gen::gen::predecode(&defs, &mut out)?;
    }

    Ok(())
}
