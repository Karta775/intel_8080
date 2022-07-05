# intel_8080 :joystick: 
An Intel 8080 emulator written in Rust as a stepping stone towards creating a GameBoy Color emulator.

## Build instructions
```shell
git clone https://github.com/Karta775/intel_8080
cd chip8_rust
cargo build
```

## Usage
```shell
cargo run -- invaders.rom -d # Disassemble invaders.rom
cargo run -- invaders.rom # Run invaders.rom on the emulator
RUST_LOG=debug cargo run -- invaders.rom # To debug the emulator
```

## Goals
I would like to make the following:
- [x] A disassembler
- [ ] An emulator capable of running at least Space Invaders
