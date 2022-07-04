use crate::disassembler::disassemble_op;
use log::{debug, error, log_enabled, info, warn};

pub struct Cpu {
    pub pc: u16,
    pub ram: Vec<u8>,
    // Registers
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    // Flags
    pub z: bool,
    pub s: bool,
    pub p: bool,
    pub cy: bool,
    pub ac: bool,
    // Debugging
    pub unimplemented: Vec<u8>,
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            pc: 0,
            ram: vec![0; 0xFFFF],
            a: 0, b: 0, c: 0, d: 0, e:0,
            z: false, s: false, p: false, cy: false, ac: false,
            unimplemented: Vec::new(),
        }
    }

    pub fn load_rom(&mut self, rom: Vec<u8>) {
        for (i, op) in rom.iter().enumerate() {
            self.ram[i] = *op;
        }
    }

    fn op_implemented(&self, instruction: &str, description: &str) {
        debug!("I ({:04x}) {:#04x} ~ {:<15} | {}",
            self.pc, self.ram[self.pc as usize], instruction, description
        );
    }

    fn op_unimplemented(&mut self, instruction: &str, description: &str) {
        self.unimplemented.push(self.ram[self.pc as usize]);
        warn!("U ({:04x}) {:#04x} ~ {:<15} | {}",
            self.pc, self.ram[self.pc as usize], instruction, description
        );
    }

    pub fn execute(&mut self) {
        let mut bytes: usize = 0;
        let mut advance_pc = true;

        // TODO: Clean up this sloppy code, maybe use a function like in the disassembler
        let byte_1 = self.ram[self.pc as usize];
        let mut byte_2: u8 = 0;
        let mut byte_3: u8 = 0;
        if self.pc <= (u16::MAX - 2) {
            byte_2 = self.ram[self.pc as usize + 1];
        }
        if self.pc <= (u16::MAX - 3) {
            byte_3 = self.ram[self.pc as usize + 2];
        }
        let two_byte = (byte_3 as u16) << 8 |  byte_2 as u16;

        match self.ram[self.pc as usize] {
            0x00 => { bytes = 1; self.op_implemented("NOP", "Do nothing"); }, // NOP
            0x01 => {
                self.op_implemented(
                    &format!("LXI B,#${:04x}", two_byte),
                    "B = left, C = right"
                );
                bytes = 3;
                self.b = byte_3;
                self.c = byte_2;
            }, // LXI B,D16
            0x02 => { bytes = 1; self.op_unimplemented("STAX B", "(BC) <- A") }, // STAX B
            0x03 => { bytes = 1; self.op_unimplemented("INX B", "BC <- BC+1") }, // INX B
            0x04 => { bytes = 1; self.op_unimplemented("INR B", "B <- B+1") }, // INR B
            0x05 => { bytes = 1; self.op_unimplemented("DCR B", "B <- B-1") }, // DCR B
            0x06 => {
                self.op_implemented(
                    &format!("MVI B,#${:02x}", byte_2),
                    "B <- byte 2"
                );
                bytes = 2;
                self.b = byte_2;
            }, // MVI B, D8
            0x07 => { bytes = 1; self.op_unimplemented("RLC", "A = A << 1; bit 0 = prev bit 7; CY = prev bit 7") }, // RLC
            0x08 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0x09 => { bytes = 1; self.op_unimplemented("DAD B", "HL = HL + BC") }, // DAD B
            0x0a => { bytes = 1; self.op_unimplemented("LDAX B", "A <- (BC)") }, // LDAX B
            0x0b => { bytes = 1; self.op_unimplemented("DCX B", "BC = BC-1") }, // DCX B
            0x0c => { bytes = 1; self.op_unimplemented("INR C", "C <- C+1") }, // INR C
            0x0d => { bytes = 1; self.op_unimplemented("DCR C", "C <-C-1") }, // DCR C
            0x0e => { bytes = 2; self.op_unimplemented("MVI C,D8", "C <- byte 2") }, // MVI C,D8
            0x0f => { bytes = 1; self.op_unimplemented("RRC", "A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0") }, // RRC
            0x10 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0x11 => {
                self.op_implemented(
                    &format!("LXI D,#${:04x}", two_byte),
                    "D <- byte 3, E <- byte 2"
                );
                bytes = 3;
                self.d = byte_3;
                self.e = byte_2;
            }, // LXI D,D16
            0x12 => { bytes = 1; self.op_unimplemented("STAX D", "(DE) <- A") }, // STAX D
            0x13 => { bytes = 1; self.op_unimplemented("INX D", "DE <- DE + 1") }, // INX D
            0x14 => { bytes = 1; self.op_unimplemented("INR D", "D <- D+1") }, // INR D
            0x15 => { bytes = 1; self.op_unimplemented("DCR D", "D <- D-1") }, // DCR D
            0x16 => { bytes = 2; self.op_unimplemented("MVI D, D8", "D <- byte 2") }, // MVI D, D8
            0x17 => { bytes = 1; self.op_unimplemented("RAL", "A = A << 1; bit 0 = prev CY; CY = prev bit 7") }, // RAL
            0x18 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0x19 => { bytes = 1; self.op_unimplemented("DAD D", "HL = HL + DE") }, // DAD D
            0x1a => { bytes = 1; self.op_unimplemented("LDAX D", "A <- (DE)") }, // LDAX D
            0x1b => { bytes = 1; self.op_unimplemented("DCX D", "DE = DE-1") }, // DCX D
            0x1c => { bytes = 1; self.op_unimplemented("INR E", "E <-E+1") }, // INR E
            0x1d => { bytes = 1; self.op_unimplemented("DCR E", "E <- E-1") }, // DCR E
            0x1e => { bytes = 2; self.op_unimplemented("MVI E,D8", "E <- byte 2") }, // MVI E,D8
            0x1f => { bytes = 1; self.op_unimplemented("RAR", "A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0") }, // RAR
            0x20 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0x21 => { bytes = 3; self.op_unimplemented("LXI H,D16", "H <- byte 3, L <- byte 2") }, // LXI H,D16
            0x22 => { bytes = 3; self.op_unimplemented("SHLD adr", "(adr) <-L; (adr+1)<-H") }, // SHLD adr
            0x23 => { bytes = 1; self.op_unimplemented("INX H", "HL <- HL + 1") }, // INX H
            0x24 => { bytes = 1; self.op_unimplemented("INR H", "H <- H+1") }, // INR H
            0x25 => { bytes = 1; self.op_unimplemented("DCR H", "H <- H-1") }, // DCR H
            0x26 => { bytes = 2; self.op_unimplemented("MVI H,D8", "H <- byte 2") }, // MVI H,D8
            0x27 => { bytes = 1; self.op_unimplemented("DAA", "special") }, // DAA
            0x28 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0x29 => { bytes = 1; self.op_unimplemented("DAD H", "HL = HL + HI") }, // DAD H
            0x2a => { bytes = 3; self.op_unimplemented("LHLD adr", "L <- (adr); H<-(adr+1)") }, // LHLD adr
            0x2b => { bytes = 1; self.op_unimplemented("DCX H", "HL = HL-1") }, // DCX H
            0x2c => { bytes = 1; self.op_unimplemented("INR L", "L <- L+1") }, // INR L
            0x2d => { bytes = 1; self.op_unimplemented("DCR L", "L <- L-1") }, // DCR L
            0x2e => { bytes = 2; self.op_unimplemented("MVI L, D8", "L <- byte 2") }, // MVI L, D8
            0x2f => { bytes = 1; self.op_unimplemented("CMA", "A <- !A") }, // CMA
            0x30 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0x31 => { bytes = 3; self.op_unimplemented("LXI SP, D16", "SP.hi <- byte 3, SP.lo <- byte 2") }, // LXI SP, D16
            0x32 => {
                self.op_implemented(
                    &format!("STA ${:04x}", two_byte),
                    "(adr) <- A"
                );
                bytes = 3;
                self.ram[two_byte as usize] = self.a;
            }, // STA adr
            0x33 => { bytes = 1; self.op_unimplemented("INX SP", "SP = SP + 1") }, // INX SP
            0x34 => { bytes = 1; self.op_unimplemented("INR M", "(HL) <- (HL)+1") }, // INR M
            0x35 => { bytes = 1; self.op_unimplemented("DCR M", "(HL) <- (HL)-1") }, // DCR M
            0x36 => { bytes = 2; self.op_unimplemented("MVI M,D8", "(HL) <- byte 2") }, // MVI M,D8
            0x37 => { bytes = 1; self.op_unimplemented("STC", "CY = 1") }, // STC
            0x38 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0x39 => { bytes = 1; self.op_unimplemented("DAD SP", "HL = HL + SP") }, // DAD SP
            0x3a => {
                self.op_implemented(
                    &format!("LDA ${:04x}", two_byte),
                    "A <- (adr)"
                );
                bytes = 3;
                self.a = self.ram[two_byte as usize];
            }, // LDA adr
            0x3b => { bytes = 1; self.op_unimplemented("DCX SP", "SP = SP-1") }, // DCX SP
            0x3c => { bytes = 1; self.op_unimplemented("INR A", "A <- A+1") }, // INR A
            0x3d => { bytes = 1; self.op_unimplemented("DCR A", "A <- A-1") }, // DCR A
            0x3e => {
                self.op_implemented(
                    &format!("MVI A,#${:02x}", byte_2),
                    "A <- byte 2"
                );
                bytes = 2;
                self.a = byte_2;
            }, // MVI A,D8
            0x3f => { bytes = 1; self.op_unimplemented("CMC", "CY=!CY") }, // CMC
            0x40 => { bytes = 1; self.op_unimplemented("MOV B,B", "B <- B") }, // MOV B,B
            0x41 => { bytes = 1; self.op_unimplemented("MOV B,C", "B <- C") }, // MOV B,C
            0x42 => { bytes = 1; self.op_unimplemented("MOV B,D", "B <- D") }, // MOV B,D
            0x43 => { bytes = 1; self.op_unimplemented("MOV B,E", "B <- E") }, // MOV B,E
            0x44 => { bytes = 1; self.op_unimplemented("MOV B,H", "B <- H") }, // MOV B,H
            0x45 => { bytes = 1; self.op_unimplemented("MOV B,L", "B <- L") }, // MOV B,L
            0x46 => { bytes = 1; self.op_unimplemented("MOV B,M", "B <- (HL)") }, // MOV B,M
            0x47 => { bytes = 1; self.op_unimplemented("MOV B,A", "B <- A") }, // MOV B,A
            0x48 => { bytes = 1; self.op_unimplemented("MOV C,B", "C <- B") }, // MOV C,B
            0x49 => { bytes = 1; self.op_unimplemented("MOV C,C", "C <- C") }, // MOV C,C
            0x4a => { bytes = 1; self.op_unimplemented("MOV C,D", "C <- D") }, // MOV C,D
            0x4b => { bytes = 1; self.op_unimplemented("MOV C,E", "C <- E") }, // MOV C,E
            0x4c => { bytes = 1; self.op_unimplemented("MOV C,H", "C <- H") }, // MOV C,H
            0x4d => { bytes = 1; self.op_unimplemented("MOV C,L", "C <- L") }, // MOV C,L
            0x4e => { bytes = 1; self.op_unimplemented("MOV C,M", "C <- (HL)") }, // MOV C,M
            0x4f => { bytes = 1; self.op_unimplemented("MOV C,A", "C <- A") }, // MOV C,A
            0x50 => { bytes = 1; self.op_unimplemented("MOV D,B", "D <- B") }, // MOV D,B
            0x51 => { bytes = 1; self.op_unimplemented("MOV D,C", "D <- C") }, // MOV D,C
            0x52 => { bytes = 1; self.op_unimplemented("MOV D,D", "D <- D") }, // MOV D,D
            0x53 => { bytes = 1; self.op_unimplemented("MOV D,E", "D <- E") }, // MOV D,E
            0x54 => { bytes = 1; self.op_unimplemented("MOV D,H", "D <- H") }, // MOV D,H
            0x55 => { bytes = 1; self.op_unimplemented("MOV D,L", "D <- L") }, // MOV D,L
            0x56 => { bytes = 1; self.op_unimplemented("MOV D,M", "D <- (HL)") }, // MOV D,M
            0x57 => { bytes = 1; self.op_unimplemented("MOV D,A", "D <- A") }, // MOV D,A
            0x58 => { bytes = 1; self.op_unimplemented("MOV E,B", "E <- B") }, // MOV E,B
            0x59 => { bytes = 1; self.op_unimplemented("MOV E,C", "E <- C") }, // MOV E,C
            0x5a => { bytes = 1; self.op_unimplemented("MOV E,D", "E <- D") }, // MOV E,D
            0x5b => { bytes = 1; self.op_unimplemented("MOV E,E", "E <- E") }, // MOV E,E
            0x5c => { bytes = 1; self.op_unimplemented("MOV E,H", "E <- H") }, // MOV E,H
            0x5d => { bytes = 1; self.op_unimplemented("MOV E,L", "E <- L") }, // MOV E,L
            0x5e => { bytes = 1; self.op_unimplemented("MOV E,M", "E <- (HL)") }, // MOV E,M
            0x5f => { bytes = 1; self.op_unimplemented("MOV E,A", "E <- A") }, // MOV E,A
            0x60 => { bytes = 1; self.op_unimplemented("MOV H,B", "H <- B") }, // MOV H,B
            0x61 => { bytes = 1; self.op_unimplemented("MOV H,C", "H <- C") }, // MOV H,C
            0x62 => { bytes = 1; self.op_unimplemented("MOV H,D", "H <- D") }, // MOV H,D
            0x63 => { bytes = 1; self.op_unimplemented("MOV H,E", "H <- E") }, // MOV H,E
            0x64 => { bytes = 1; self.op_unimplemented("MOV H,H", "H <- H") }, // MOV H,H
            0x65 => { bytes = 1; self.op_unimplemented("MOV H,L", "H <- L") }, // MOV H,L
            0x66 => { bytes = 1; self.op_unimplemented("MOV H,M", "H <- (HL)") }, // MOV H,M
            0x67 => { bytes = 1; self.op_unimplemented("MOV H,A", "H <- A") }, // MOV H,A
            0x68 => { bytes = 1; self.op_unimplemented("MOV L,B", "L <- B") }, // MOV L,B
            0x69 => { bytes = 1; self.op_unimplemented("MOV L,C", "L <- C") }, // MOV L,C
            0x6a => { bytes = 1; self.op_unimplemented("MOV L,D", "L <- D") }, // MOV L,D
            0x6b => { bytes = 1; self.op_unimplemented("MOV L,E", "L <- E") }, // MOV L,E
            0x6c => { bytes = 1; self.op_unimplemented("MOV L,H", "L <- H") }, // MOV L,H
            0x6d => { bytes = 1; self.op_unimplemented("MOV L,L", "L <- L") }, // MOV L,L
            0x6e => { bytes = 1; self.op_unimplemented("MOV L,M", "L <- (HL)") }, // MOV L,M
            0x6f => { bytes = 1; self.op_unimplemented("MOV L,A", "L <- A") }, // MOV L,A
            0x70 => { bytes = 1; self.op_unimplemented("MOV M,B", "(HL) <- B") }, // MOV M,B
            0x71 => { bytes = 1; self.op_unimplemented("MOV M,C", "(HL) <- C") }, // MOV M,C
            0x72 => { bytes = 1; self.op_unimplemented("MOV M,D", "(HL) <- D") }, // MOV M,D
            0x73 => { bytes = 1; self.op_unimplemented("MOV M,E", "(HL) <- E") }, // MOV M,E
            0x74 => { bytes = 1; self.op_unimplemented("MOV M,H", "(HL) <- H") }, // MOV M,H
            0x75 => { bytes = 1; self.op_unimplemented("MOV M,L", "(HL) <- L") }, // MOV M,L
            0x76 => { bytes = 1; self.op_unimplemented("HLT", "special") }, // HLT
            0x77 => { bytes = 1; self.op_unimplemented("MOV M,A", "(HL) <- A") }, // MOV M,A
            0x78 => { bytes = 1; self.op_unimplemented("MOV A,B", "A <- B") }, // MOV A,B
            0x79 => { bytes = 1; self.op_unimplemented("MOV A,C", "A <- C") }, // MOV A,C
            0x7a => { bytes = 1; self.op_unimplemented("MOV A,D", "A <- D") }, // MOV A,D
            0x7b => { bytes = 1; self.op_unimplemented("MOV A,E", "A <- E") }, // MOV A,E
            0x7c => { bytes = 1; self.op_unimplemented("MOV A,H", "A <- H") }, // MOV A,H
            0x7d => { bytes = 1; self.op_unimplemented("MOV A,L", "A <- L") }, // MOV A,L
            0x7e => { bytes = 1; self.op_unimplemented("MOV A,M", "A <- (HL)") }, // MOV A,M
            0x7f => { bytes = 1; self.op_unimplemented("MOV A,A", "A <- A") }, // MOV A,A
            0x80 => { bytes = 1; self.op_unimplemented("ADD B", "A <- A + B") }, // ADD B
            0x81 => { bytes = 1; self.op_unimplemented("ADD C", "A <- A + C") }, // ADD C
            0x82 => { bytes = 1; self.op_unimplemented("ADD D", "A <- A + D") }, // ADD D
            0x83 => { bytes = 1; self.op_unimplemented("ADD E", "A <- A + E") }, // ADD E
            0x84 => { bytes = 1; self.op_unimplemented("ADD H", "A <- A + H") }, // ADD H
            0x85 => { bytes = 1; self.op_unimplemented("ADD L", "A <- A + L") }, // ADD L
            0x86 => { bytes = 1; self.op_unimplemented("ADD M", "A <- A + (HL)") }, // ADD M
            0x87 => { bytes = 1; self.op_unimplemented("ADD A", "A <- A + A") }, // ADD A
            0x88 => { bytes = 1; self.op_unimplemented("ADC B", "A <- A + B + CY") }, // ADC B
            0x89 => { bytes = 1; self.op_unimplemented("ADC C", "A <- A + C + CY") }, // ADC C
            0x8a => { bytes = 1; self.op_unimplemented("ADC D", "A <- A + D + CY") }, // ADC D
            0x8b => { bytes = 1; self.op_unimplemented("ADC E", "A <- A + E + CY") }, // ADC E
            0x8c => { bytes = 1; self.op_unimplemented("ADC H", "A <- A + H + CY") }, // ADC H
            0x8d => { bytes = 1; self.op_unimplemented("ADC L", "A <- A + L + CY") }, // ADC L
            0x8e => { bytes = 1; self.op_unimplemented("ADC M", "A <- A + (HL) + CY") }, // ADC M
            0x8f => { bytes = 1; self.op_unimplemented("ADC A", "A <- A + A + CY") }, // ADC A
            0x90 => { bytes = 1; self.op_unimplemented("SUB B", "A <- A - B") }, // SUB B
            0x91 => { bytes = 1; self.op_unimplemented("SUB C", "A <- A - C") }, // SUB C
            0x92 => { bytes = 1; self.op_unimplemented("SUB D", "A <- A + D") }, // SUB D
            0x93 => { bytes = 1; self.op_unimplemented("SUB E", "A <- A - E") }, // SUB E
            0x94 => { bytes = 1; self.op_unimplemented("SUB H", "A <- A + H") }, // SUB H
            0x95 => { bytes = 1; self.op_unimplemented("SUB L", "A <- A - L") }, // SUB L
            0x96 => { bytes = 1; self.op_unimplemented("SUB M", "A <- A + (HL)") }, // SUB M
            0x97 => { bytes = 1; self.op_unimplemented("SUB A", "A <- A - A") }, // SUB A
            0x98 => { bytes = 1; self.op_unimplemented("SBB B", "A <- A - B - CY") }, // SBB B
            0x99 => { bytes = 1; self.op_unimplemented("SBB C", "A <- A - C - CY") }, // SBB C
            0x9a => { bytes = 1; self.op_unimplemented("SBB D", "A <- A - D - CY") }, // SBB D
            0x9b => { bytes = 1; self.op_unimplemented("SBB E", "A <- A - E - CY") }, // SBB E
            0x9c => { bytes = 1; self.op_unimplemented("SBB H", "A <- A - H - CY") }, // SBB H
            0x9d => { bytes = 1; self.op_unimplemented("SBB L", "A <- A - L - CY") }, // SBB L
            0x9e => { bytes = 1; self.op_unimplemented("SBB M", "A <- A - (HL) - CY") }, // SBB M
            0x9f => { bytes = 1; self.op_unimplemented("SBB A", "A <- A - A - CY") }, // SBB A
            0xa0 => { bytes = 1; self.op_unimplemented("ANA B", "A <- A & B") }, // ANA B
            0xa1 => { bytes = 1; self.op_unimplemented("ANA C", "A <- A & C") }, // ANA C
            0xa2 => { bytes = 1; self.op_unimplemented("ANA D", "A <- A & D") }, // ANA D
            0xa3 => { bytes = 1; self.op_unimplemented("ANA E", "A <- A & E") }, // ANA E
            0xa4 => { bytes = 1; self.op_unimplemented("ANA H", "A <- A & H") }, // ANA H
            0xa5 => { bytes = 1; self.op_unimplemented("ANA L", "A <- A & L") }, // ANA L
            0xa6 => { bytes = 1; self.op_unimplemented("ANA M", "A <- A & (HL)") }, // ANA M
            0xa7 => { bytes = 1; self.op_unimplemented("ANA A", "A <- A & A") }, // ANA A
            0xa8 => { bytes = 1; self.op_unimplemented("XRA B", "A <- A ^ B") }, // XRA B
            0xa9 => { bytes = 1; self.op_unimplemented("XRA C", "A <- A ^ C") }, // XRA C
            0xaa => { bytes = 1; self.op_unimplemented("XRA D", "A <- A ^ D") }, // XRA D
            0xab => { bytes = 1; self.op_unimplemented("XRA E", "A <- A ^ E") }, // XRA E
            0xac => { bytes = 1; self.op_unimplemented("XRA H", "A <- A ^ H") }, // XRA H
            0xad => { bytes = 1; self.op_unimplemented("XRA L", "A <- A ^ L") }, // XRA L
            0xae => { bytes = 1; self.op_unimplemented("XRA M", "A <- A ^ (HL)") }, // XRA M
            0xaf => { bytes = 1; self.op_unimplemented("XRA A", "A <- A ^ A") }, // XRA A
            0xb0 => { bytes = 1; self.op_unimplemented("ORA B", "A <- A | B") }, // ORA B
            0xb1 => { bytes = 1; self.op_unimplemented("ORA C", "A <- A | C") }, // ORA C
            0xb2 => { bytes = 1; self.op_unimplemented("ORA D", "A <- A | D") }, // ORA D
            0xb3 => { bytes = 1; self.op_unimplemented("ORA E", "A <- A | E") }, // ORA E
            0xb4 => { bytes = 1; self.op_unimplemented("ORA H", "A <- A | H") }, // ORA H
            0xb5 => { bytes = 1; self.op_unimplemented("ORA L", "A <- A | L") }, // ORA L
            0xb6 => { bytes = 1; self.op_unimplemented("ORA M", "A <- A | (HL)") }, // ORA M
            0xb7 => { bytes = 1; self.op_unimplemented("ORA A", "A <- A | A") }, // ORA A
            0xb8 => { bytes = 1; self.op_unimplemented("CMP B", "A - B") }, // CMP B
            0xb9 => { bytes = 1; self.op_unimplemented("CMP C", "A - C") }, // CMP C
            0xba => { bytes = 1; self.op_unimplemented("CMP D", "A - D") }, // CMP D
            0xbb => { bytes = 1; self.op_unimplemented("CMP E", "A - E") }, // CMP E
            0xbc => { bytes = 1; self.op_unimplemented("CMP H", "A - H") }, // CMP H
            0xbd => { bytes = 1; self.op_unimplemented("CMP L", "A - L") }, // CMP L
            0xbe => { bytes = 1; self.op_unimplemented("CMP M", "A - (HL)") }, // CMP M
            0xbf => { bytes = 1; self.op_unimplemented("CMP A", "A - A") }, // CMP A
            0xc0 => { bytes = 1; self.op_unimplemented("RNZ", "if NZ, RET") }, // RNZ
            0xc1 => { bytes = 1; self.op_unimplemented("POP B", "C <- (sp); B <- (sp+1); sp <- sp+2") }, // POP B
            0xc2 => {
                self.op_implemented(
                    &format!("JNZ ${:04x}", two_byte),
                    "if NZ, PC <- adr"
                );
                bytes = 3;
                if !self.z { self.pc = two_byte };
            }, // JNZ adr
            0xc3 => {
                self.op_implemented(
                    &format!("JMP ${:04x}", two_byte),
                    "Jump to an address"
                );
                bytes = 3;
                advance_pc = false;
                self.pc = two_byte;
            }, // JMP adr            0xc4 => { cycles = 3; self.op_unimplemented("CNZ adr", "if NZ, CALL adr") }, // CNZ adr
            0xc5 => { bytes = 1; self.op_unimplemented("PUSH B", "(sp-2)<-C; (sp-1)<-B; sp <- sp - 2") }, // PUSH B
            0xc6 => {
                self.op_implemented(
                    &format!("ADI #${:02x}", byte_2),
                    "A <- A + byte"
                );
                bytes = 2;
                self.a += byte_2;
            }, // ADI D8
            0xc7 => { bytes = 1; self.op_unimplemented("RST 0", "CALL $0") }, // RST 0
            0xc8 => { bytes = 1; self.op_unimplemented("RZ", "if Z, RET") }, // RZ
            0xc9 => { bytes = 1; self.op_unimplemented("RET", "PC.lo <- (sp); PC.hi<-(sp+1); SP <- SP+2") }, // RET
            0xca => {
                self.op_implemented(
                    &format!("JZ ${:04x}", two_byte),
                    "if Z, PC <- adr"
                );
                bytes = 3;
                advance_pc = false;
                if self.z { self.pc = two_byte; };
            }, // JZ adr
            0xcb => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0xcc => { bytes = 3; self.op_unimplemented("CZ adr", "if Z, CALL adr") }, // CZ adr
            0xcd => { bytes = 3; self.op_unimplemented("CALL adr", "(SP-1)<-PC.hi;(SP-2)<-PC.lo;SP<-SP-2;PC=adr") }, // CALL adr
            0xce => { bytes = 2; self.op_unimplemented("ACI D8", "A <- A + data + CY") }, // ACI D8
            0xcf => { bytes = 1; self.op_unimplemented("RST 1", "CALL $8") }, // RST 1
            0xd0 => { bytes = 1; self.op_unimplemented("RNC", "if NCY, RET") }, // RNC
            0xd1 => { bytes = 1; self.op_unimplemented("POP D", "E <- (sp); D <- (sp+1); sp <- sp+2") }, // POP D
            0xd2 => { bytes = 3; self.op_unimplemented("JNC adr", "if NCY, PC<-adr") }, // JNC adr
            0xd3 => { bytes = 2; self.op_unimplemented("OUT D8", "special") }, // OUT D8
            0xd4 => { bytes = 3; self.op_unimplemented("CNC adr", "if NCY, CALL adr") }, // CNC adr
            0xd5 => { bytes = 1; self.op_unimplemented("PUSH D", "(sp-2)<-E; (sp-1)<-D; sp <- sp - 2") }, // PUSH D
            0xd6 => { bytes = 2; self.op_unimplemented("SUI D8", "A <- A - data") }, // SUI D8
            0xd7 => { bytes = 1; self.op_unimplemented("RST 2", "CALL $10") }, // RST 2
            0xd8 => { bytes = 1; self.op_unimplemented("RC", "if CY, RET") }, // RC
            0xd9 => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0xda => { bytes = 3; self.op_unimplemented("JC adr", "if CY, PC<-adr") }, // JC adr
            0xdb => { bytes = 2; self.op_unimplemented("IN D8", "special") }, // IN D8
            0xdc => { bytes = 3; self.op_unimplemented("CC adr", "if CY, CALL adr") }, // CC adr
            0xdd => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0xde => { bytes = 2; self.op_unimplemented("SBI D8", "A <- A - data - CY") }, // SBI D8
            0xdf => { bytes = 1; self.op_unimplemented("RST 3", "CALL $18") }, // RST 3
            0xe0 => { bytes = 1; self.op_unimplemented("RPO", "if PO, RET") }, // RPO
            0xe1 => { bytes = 1; self.op_unimplemented("POP H", "L <- (sp); H <- (sp+1); sp <- sp+2") }, // POP H
            0xe2 => { bytes = 3; self.op_unimplemented("JPO adr", "if PO, PC <- adr") }, // JPO adr
            0xe3 => { bytes = 1; self.op_unimplemented("XTHL", "L <-> (SP); H <-> (SP+1) ") }, // XTHL
            0xe4 => { bytes = 3; self.op_unimplemented("CPO adr", "if PO, CALL adr") }, // CPO adr
            0xe5 => { bytes = 1; self.op_unimplemented("PUSH H", "(sp-2)<-L; (sp-1)<-H; sp <- sp - 2") }, // PUSH H
            0xe6 => { bytes = 2; self.op_unimplemented("ANI D8", "A <- A & data") }, // ANI D8
            0xe7 => { bytes = 1; self.op_unimplemented("RST 4", "CALL $20") }, // RST 4
            0xe8 => { bytes = 1; self.op_unimplemented("RPE", "if PE, RET") }, // RPE
            0xe9 => { bytes = 1; self.op_unimplemented("PCHL", "PC.hi <- H; PC.lo <- L") }, // PCHL
            0xea => { bytes = 3; self.op_unimplemented("JPE adr", "if PE, PC <- adr") }, // JPE adr
            0xeb => { bytes = 1; self.op_unimplemented("XCHG", "H <-> D; L <-> E") }, // XCHG
            0xec => { bytes = 3; self.op_unimplemented("CPE adr", "if PE, CALL adr") }, // CPE adr
            0xed => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0xee => { bytes = 2; self.op_unimplemented("XRI D8", "A <- A ^ data") }, // XRI D8
            0xef => { bytes = 1; self.op_unimplemented("RST 5", "CALL $28") }, // RST 5
            0xf0 => { bytes = 1; self.op_unimplemented("RP", "if P, RET") }, // RP
            0xf1 => { bytes = 1; self.op_unimplemented("POP PSW", "flags <- (sp); A <- (sp+1); sp <- sp+2") }, // POP PSW
            0xf2 => { bytes = 3; self.op_unimplemented("JP adr", "if P=1 PC <- adr") }, // JP adr
            0xf3 => { bytes = 1; self.op_unimplemented("DI", "special") }, // DI
            0xf4 => { bytes = 3; self.op_unimplemented("CP adr", "if P, PC <- adr") }, // CP adr
            0xf5 => { bytes = 1; self.op_unimplemented("PUSH PSW", "(sp-2)<-flags; (sp-1)<-A; sp <- sp - 2") }, // PUSH PSW
            0xf6 => { bytes = 2; self.op_unimplemented("ORI D8", "A <- A | data") }, // ORI D8
            0xf7 => { bytes = 1; self.op_unimplemented("RST 6", "CALL $30") }, // RST 6
            0xf8 => { bytes = 1; self.op_unimplemented("RM", "if M, RET") }, // RM
            0xf9 => { bytes = 1; self.op_unimplemented("SPHL", "SP=HL") }, // SPHL
            0xfa => { bytes = 3; self.op_unimplemented("JM adr", "if M, PC <- adr") }, // JM adr
            0xfb => { bytes = 1; self.op_unimplemented("EI", "special") }, // EI
            0xfc => { bytes = 3; self.op_unimplemented("CM adr", "if M, CALL adr") }, // CM adr
            0xfd => { bytes = 1; self.op_unimplemented("-", "") }, // -
            0xfe => { bytes = 2; self.op_unimplemented("CPI D8", "A - data") }, // CPI D8
            0xff => { bytes = 1; self.op_unimplemented("RST 7", "CALL $38") }, // RST 7
            _ => () // Unfortunately CLion's Rust plugin requires this although it compiles fine without.
        }
        if advance_pc {
            self.pc += bytes as u16;
        }
    }
}