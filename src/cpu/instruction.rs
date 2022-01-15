use anyhow::{bail, Result};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;
use std::fmt;

use super::debug::InstructionDisplay;

#[derive(Debug, Copy, Clone, Primitive)]
pub enum R8 {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    HLInd = 6,
    A = 7,
}

#[derive(Debug, Copy, Clone, Primitive)]
pub enum Word {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
}

#[derive(Debug, Copy, Clone, Primitive)]
pub enum Indirect {
    BCInd = 0,      // (BC)
    DEInd = 1,      // (DE)
    HLIndPlus = 2,  // (HL+)
    HLIndMinus = 3, // (HL-)
    CInd = 4,       // (FF00 + C)
}

#[derive(Debug, Copy, Clone, Primitive)]
pub enum PushPopTarget {
    BC = 0,
    DE = 1,
    HL = 2,
    AF = 3,
}

#[derive(Debug, Copy, Clone, Primitive)]
pub enum BranchCondition {
    NotZero = 0,
    Zero = 1,
    NotCarry = 2,
    Carry = 3,
    Always = 4,
}

#[derive(Debug, Copy, Clone, Primitive)]
pub enum BitPosition {
    Zero = 0,
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
    Seven = 7,
}

#[derive(Debug, Copy, Clone)]
pub enum LDType {
    // 92 total LD instrs
    MoveByte(R8, R8),   // (63) LD r, r
    AFromInd(Indirect), //  (5) LD A, (XX)
    IndFromA(Indirect), //  (5) LD (xx), A
    ByteImm(R8),        //  (8) LD r, u8
    WordImm(Word),      //  (4) LD ww, u16
    AFromByteInd,       //  (1) LD A, (FF00+u8)
    ByteIndFromA,       //  (1) LD (FF00+u8), A
    AFromAddr,          //  (1) LD A, (u16)
    AddrFromA,          //  (1) LD (u16), A
    SPFromHL,           //  (1) LD SP, HL
    HLFromSPi8,         //  (1) LD HL, SP + i8
    AddrFromSP,         //  (1) LD (u16), SP
}

#[derive(Debug, Copy, Clone)]
pub enum ALUType {
    R8(R8),
    Imm8,
}

#[derive(Debug, Copy, Clone)]
pub enum IncDecTarget {
    R8(R8),
    Word(Word),
}

#[derive(Debug)]
pub enum OP {
    LD(LDType),

    AND(ALUType),
    OR(ALUType),
    XOR(ALUType),
    ADD(ALUType),
    ADC(ALUType),
    ADDHL(Word),
    ADDSPi8,
    SUB(ALUType),
    SBC(ALUType),
    CP(ALUType),
    DAA,
    CPL,
    SCF,
    CCF,

    INC(IncDecTarget),
    DEC(IncDecTarget),

    BIT(BitPosition, R8),
    RES(BitPosition, R8),
    SET(BitPosition, R8),

    RL(R8),
    RR(R8),
    RLC(R8),
    RRC(R8),
    SLA(R8),
    SRA(R8),
    SRL(R8),
    SWAP(R8),
    RLA,
    RRA,
    RLCA,
    RRCA,

    JR(BranchCondition),
    JP(BranchCondition),
    JPHL,
    CALL(BranchCondition),
    RST(u16),
    RET(BranchCondition),
    RETI,

    PUSH(PushPopTarget),
    POP(PushPopTarget),

    NOP,
    DI,
    EI,
    STOP,
    HALT,
}

impl OP {
    pub fn from_byte(byte: u8) -> Result<(Self, (u8, u64))> {
        let b1 = (byte >> 3) & 0b111;
        let b2 = (byte >> 3) & 0b11;
        let b3 = (byte >> 4) & 0b11;
        let w = Word::from_u8(b3).unwrap();
        let ind = Indirect::from_u8(b3).unwrap();
        let r81 = R8::from_u8(b1).unwrap();
        let r82 = R8::from_u8(byte & 0b111).unwrap();
        let branch = BranchCondition::from_u8(b2).unwrap();
        let push_pop = PushPopTarget::from_u8(b3).unwrap();
        Ok(match byte {
            0x01 | 0x11 | 0x21 | 0x31 => (OP::LD(LDType::WordImm(w)), (3, 12)),
            0x02 | 0x12 | 0x22 | 0x32 => (OP::LD(LDType::IndFromA(ind)), (1, 8)),
            0x03 | 0x13 | 0x23 | 0x33 => (OP::INC(IncDecTarget::Word(w)), (1, 8)),
            0x04 | 0x14 | 0x24 | 0x34 | 0x0c | 0x1c | 0x2c | 0x3c => (
                OP::INC(IncDecTarget::R8(r81)),
                OP::r8_len_cycles(r81, (1, 4), 12),
            ),
            0x05 | 0x15 | 0x25 | 0x35 | 0x0d | 0x1d | 0x2d | 0x3d => (
                OP::DEC(IncDecTarget::R8(r81)),
                OP::r8_len_cycles(r81, (1, 4), 12),
            ),
            0x06 | 0x16 | 0x26 | 0x36 | 0x0e | 0x1e | 0x2e | 0x3e => (
                OP::LD(LDType::ByteImm(r81)),
                OP::r8_len_cycles(r81, (2, 8), 12),
            ),
            0x09 | 0x19 | 0x29 | 0x39 => (OP::ADDHL(w), (1, 8)),
            0x0a | 0x1a | 0x2a | 0x3a => (OP::LD(LDType::AFromInd(ind)), (1, 8)),
            0x0b | 0x1b | 0x2b | 0x3b => (OP::DEC(IncDecTarget::Word(w)), (1, 8)),
            0x20 | 0x30 | 0x28 | 0x38 => (OP::JR(branch), (2, 8)),
            0x40..=0x75 | 0x77..=0x7f => (
                OP::LD(LDType::MoveByte(r81, r82)),
                match (r81, r82) {
                    (R8::HLInd, _) | (_, R8::HLInd) => (1, 8),
                    (_, _) => (1, 4),
                },
            ),
            0x80..=0xbf => OP::decode_alu_op(b1, ALUType::R8(r82))?,
            0xc0 | 0xd0 | 0xc8 | 0xd8 => (OP::RET(branch), (1, 8)),
            0xc1 | 0xd1 | 0xe1 | 0xf1 => (OP::POP(push_pop), (1, 12)),
            0xc2 | 0xd2 | 0xca | 0xda => (OP::JP(branch), (3, 12)),
            0xc4 | 0xd4 | 0xcc | 0xdc => (OP::CALL(branch), (3, 12)),
            0xc5 | 0xd5 | 0xe5 | 0xf5 => (OP::PUSH(push_pop), (1, 16)),
            0xc6 | 0xd6 | 0xe6 | 0xf6 | 0xce | 0xde | 0xee | 0xfe => {
                OP::decode_alu_op(b1, ALUType::Imm8)?
            }
            0xc7 | 0xd7 | 0xe7 | 0xf7 | 0xcf | 0xdf | 0xef | 0xff => {
                let addr = b1 << 3;
                match addr {
                    0x00 | 0x08 | 0x10 | 0x18 | 0x20 | 0x28 | 0x30 | 0x38 => {
                        (OP::RST(addr as u16), (1, 16))
                    }
                    _ => bail!("Invalid RST addr: {:#02x}", addr),
                }
            }

            0x00 => (OP::NOP, (1, 4)),
            0x07 => (OP::RLCA, (1, 4)),
            0x08 => (OP::LD(LDType::AddrFromSP), (3, 20)),
            0x10 => (OP::STOP, (2, 4)),
            0x0f => (OP::RRCA, (1, 4)),
            0x17 => (OP::RLA, (1, 4)),
            0x1f => (OP::RRA, (1, 4)),
            0x18 => (OP::JR(BranchCondition::Always), (2, 8)),
            0x27 => (OP::DAA, (1, 4)),
            0x2f => (OP::CPL, (1, 4)),
            0x37 => (OP::SCF, (1, 4)),
            0x3f => (OP::CCF, (1, 4)),
            0x76 => (OP::HALT, (1, 4)),
            0xc3 => (OP::JP(BranchCondition::Always), (3, 12)),
            0xc9 => (OP::RET(BranchCondition::Always), (1, 4)),
            0xcd => (OP::CALL(BranchCondition::Always), (3, 12)),
            0xd9 => (OP::RETI, (1, 16)),
            0xe0 => (OP::LD(LDType::ByteIndFromA), (2, 12)),
            0xe2 => (OP::LD(LDType::IndFromA(Indirect::CInd)), (1, 8)),
            0xe8 => (OP::ADDSPi8, (2, 16)),
            0xe9 => (OP::JPHL, (1, 4)),
            0xea => (OP::LD(LDType::AddrFromA), (3, 16)),
            0xf0 => (OP::LD(LDType::AFromByteInd), (2, 12)),
            0xf2 => (OP::LD(LDType::AFromInd(Indirect::CInd)), (1, 8)),
            0xf3 => (OP::DI, (1, 4)),
            0xf8 => (OP::LD(LDType::HLFromSPi8), (2, 12)),
            0xf9 => (OP::LD(LDType::SPFromHL), (1, 8)),
            0xfa => (OP::LD(LDType::AFromAddr), (3, 16)),
            0xfb => (OP::EI, (1, 4)),

            0xcb => bail!("Somehow didn't catch prefix."),
            0xd3 | 0xdb | 0xdd | 0xe3 | 0xe4 | 0xeb | 0xec | 0xed | 0xf4 | 0xfc | 0xfd => {
                bail!("Invalid opcode: {:02x}", byte)
            }
        })
    }

    pub fn from_prefix_byte(byte: u8) -> (Self, (u8, u64)) {
        let bit = (byte >> 3) & 0b111;
        let bit = BitPosition::from_u8(bit).unwrap();
        let r8 = R8::from_u8(byte & 0b111).unwrap();
        let hl_cycles = match byte {
            0x40..=0x7f => 12,
            _ => 16,
        };
        let len_cycles = OP::r8_len_cycles(r8, (2, 8), hl_cycles);
        match byte {
            0x00..=0x07 => (OP::RLC(r8), len_cycles),
            0x08..=0x0f => (OP::RRC(r8), len_cycles),
            0x10..=0x17 => (OP::RL(r8), len_cycles),
            0x18..=0x1f => (OP::RR(r8), len_cycles),
            0x20..=0x27 => (OP::SLA(r8), len_cycles),
            0x28..=0x2f => (OP::SRA(r8), len_cycles),
            0x30..=0x37 => (OP::SWAP(r8), len_cycles),
            0x38..=0x3f => (OP::SRL(r8), len_cycles),
            0x40..=0x7f => (OP::BIT(bit, r8), len_cycles),
            0x80..=0xbf => (OP::RES(bit, r8), len_cycles),
            0xc0..=0xff => (OP::SET(bit, r8), len_cycles),
        }
    }

    fn r8_len_cycles(arg: R8, base: (u8, u64), hl_cycles: u64) -> (u8, u64) {
        let (len, cycles) = base;
        match arg {
            R8::HLInd => (len, hl_cycles),
            _ => (len, cycles),
        }
    }

    fn decode_alu_op(op: u8, arg: ALUType) -> Result<(Self, (u8, u64))> {
        let len_cycles = match arg {
            ALUType::R8(r8) => match r8 {
                R8::HLInd => (1, 8),
                _ => (1, 4),
            },
            ALUType::Imm8 => (2, 8),
        };
        Ok(match op {
            0 => (OP::ADD(arg), len_cycles),
            1 => (OP::ADC(arg), len_cycles),
            2 => (OP::SUB(arg), len_cycles),
            3 => (OP::SBC(arg), len_cycles),
            4 => (OP::AND(arg), len_cycles),
            5 => (OP::XOR(arg), len_cycles),
            6 => (OP::OR(arg), len_cycles),
            7 => (OP::CP(arg), len_cycles),
            _ => bail!("Invalid ALU op: {}", op),
        })
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub op: OP,
    pub cycles: u64,
    len: u8,
    bytes: Vec<u8>,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        assert!(self.bytes.len() == self.len as usize);
        let bytes = self
            .bytes
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect::<Vec<String>>()
            .join(" ");
        write!(
            f,
            "{bytes: >8} -> {:<14} [{}, {: >2}]",
            self.op.to_instr_string(&self.bytes),
            self.len,
            self.cycles
        )
    }
}

impl Instruction {
    pub fn new(op: OP, cycles: u64, bytes: Vec<u8>) -> Self {
        Instruction {
            op,
            cycles,
            len: bytes.len() as u8,
            bytes,
        }
    }
    pub fn byte_arg(&self) -> u8 {
        self.bytes[self.bytes.len() - 1]
    }

    pub fn word_arg(&self) -> u16 {
        ((self.bytes[self.bytes.len() - 1] as u16) << 8) | (self.bytes[self.bytes.len() - 2] as u16)
    }
}
