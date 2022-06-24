mod disassembler;

use std::fs::read;
use std::path::Path;
use clap::Parser;
use crate::disassembler::disassemble_ops;

/// An Intel 8080 emulator and disassembler
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The filename of the ROM
    #[clap(value_parser, required=true)]
    filename: String,

    /// Disassemble the ROM
    #[clap(short, long, value_parser)]
    disassemble: bool,
}

fn main() {
    let args = Args::parse();
    let rom_file = Path::new(&args.filename);
    let rom: Vec<u8> = read(rom_file).expect("Couldn't load the rom file");

    if args.disassemble {
        disassemble_ops(&rom);
    } else {
        println!("I'm the emulator and I've been asked to run {}", args.filename);
    }
}