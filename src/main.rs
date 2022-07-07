mod disassembler;
mod cpu;

use std::fs::read;
use std::path::Path;
use clap::Parser;
use crate::disassembler::disassemble_ops;
use crate::cpu::Cpu;

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
    env_logger::init();
    let rom_file = Path::new(&args.filename);
    let rom: Vec<u8> = read(rom_file).expect("Couldn't load the rom file");

    if args.disassemble {
        disassemble_ops(&rom);
    } else {
        println!("Starting execution cycle...");
        let mut cpu = Cpu::new();
        cpu.load_rom(rom);
        // 'running: loop {
        for i in 0..100000 {
            if cpu.pc == 65535 { break } // TODO: Artificial limit, delete when implemented properly
            cpu.execute();
        }
        cpu.unimplemented.sort_unstable();
        cpu.unimplemented.dedup();
        println!("I encountered {} unimplemented opcodes", cpu.unimplemented.len());
        println!("Goodbye...")
    }
}