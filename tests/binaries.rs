extern crate rs80;

use std::io;

fn contains_substring(haystack: &[u8], needle: &[u8]) -> bool {
    haystack.windows(needle.len()).position(|w| w == needle).is_some()
}

fn run_image(image: &[u8]) -> (u16, Vec<u8>) {
    let mut emu = rs80::Emu::default();
    emu.mem[0x100..(0x100 + image.len())].copy_from_slice(image);
    let mut out = io::Cursor::new(vec![]);

    match rs80::run_bdos(&mut emu, &mut out) {
        Ok(addr) => (addr, out.into_inner()),
        Err(e) => panic!("Unexpected error running test: {:?}", e),
    }
}

#[test]
fn _8080pre() {
    let (addr, out) = run_image(include_bytes!("8080PRE.COM"));
    assert_eq!(addr, 0x032F);
    assert!(contains_substring(&out, b"Preliminary tests complete"));
}

#[test]
fn tst8080() {
    let (addr, out) = run_image(include_bytes!("TST8080.COM"));
    assert_eq!(addr, 0x06BA);
    assert!(contains_substring(&out, b"CPU IS OPERATIONAL"));
}

#[test]
fn cputest() {
    let (addr, out) = run_image(include_bytes!("CPUTEST.COM"));
    assert_eq!(addr, 0x3B25);
    assert!(contains_substring(&out, b"CPU TESTS OK"));
    assert!(!contains_substring(&out, b"ERROR"));
    assert!(!contains_substring(&out, b"FAILED"));
}

#[test]
fn _8080exm() {
    let (addr, out) = run_image(include_bytes!("8080EXM.COM"));
    assert_eq!(addr, 0x0137);
    assert!(contains_substring(&out, b"Tests complete"));
    assert!(!contains_substring(&out, b"ERROR"));
}


