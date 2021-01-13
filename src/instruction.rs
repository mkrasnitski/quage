#![allow(dead_code)]
use anyhow::{bail, Result};
use std::fmt;

#[derive(Debug, Copy, Clone)]
pub enum Reg {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, Copy, Clone)]
pub enum Word {
    BC,
    DE,
    HL,
    SP,
}

#[derive(Debug, Copy, Clone)]
pub enum Indirect {
    CInd,       // (FF00 + C)
    BCInd,      // (BC)
    DEInd,      // (DE)
    HLIndPlus,  // (HL+)
    HLIndMinus, // (HL-)
}

#[derive(Debug, Copy, Clone)]
pub enum BitPosition {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
}

#[derive(Debug)]
pub enum LDType {
    // 92 total LD instrs
    MoveByte(Reg, Reg), // (49) LD r, r
    FromHLInd(Reg),     //  (7) LD r, (HL)
    ToHLInd(Reg),       //  (7) LD (HL), r
    AFromInd(Indirect), //  (5) LD A, (XX)
    IndFromA(Indirect), //  (5) LD (xx), A
    ByteImm(Reg),       //  (7) LD r, u8
    WordImm(Word),      //  (4) LD ww, u16
    AFromByteInd,       //  (1) LD A, (FF00+u8)
    ByteIndFromA,       //  (1) LD (FF00+u8), A
    AFromWordInd,       //  (1) LD A, (u16)
    WordIndFromA,       //  (1) LD (u16), A
    HLFromByteInd,      //  (1) LD (HL), u8
    SPFromHL,           //  (1) LD SP, HL
    HLFromSPi8,         //  (1) LD HL, SP + i8
    AddrFromSP,         //  (1) LD (u16), SP
}

#[derive(Debug, Copy, Clone)]
pub enum ArithType {
    Register(Reg),
    HLIndirect,
    Imm8,
}

#[derive(Debug, Copy, Clone)]
pub enum JPType {
    Zero,
    NotZero,
    Carry,
    NotCarry,
    Always,
}

#[derive(Debug)]
pub enum IncDecTarget {
    Register(Reg),
    Word(Word),
    HLIndirect,
}

#[derive(Debug, Copy, Clone)]
pub enum BaseTarget {
    Register(Reg),
    HLIndirect,
}

#[derive(Debug)]
pub enum OP {
    LD(LDType),

    AND(ArithType),
    OR(ArithType),
    XOR(ArithType),

    INC(BaseTarget),
    INCWord(Word),
    DEC(BaseTarget),
    DECWord(Word),

    BIT(BitPosition, BaseTarget),

    JR(JPType),

    NOP,
}

impl OP {
    pub fn from_byte(byte: u8) -> Result<Self> {
        let op = match byte {
            0x00 => OP::NOP,
            0x0c => OP::INC(BaseTarget::Register(Reg::C)),
            0x0e => OP::LD(LDType::ByteImm(Reg::C)),
            0x11 => OP::LD(LDType::WordImm(Word::DE)),
            0x1a => OP::LD(LDType::AFromInd(Indirect::DEInd)),
            0x20 => OP::JR(JPType::NotZero),
            0x21 => OP::LD(LDType::WordImm(Word::HL)),
            0x31 => OP::LD(LDType::WordImm(Word::SP)),
            0x3e => OP::LD(LDType::ByteImm(Reg::A)),
            0x32 => OP::LD(LDType::IndFromA(Indirect::HLIndMinus)),
            0x77 => OP::LD(LDType::ToHLInd(Reg::A)),
            0xe0 => OP::LD(LDType::ByteIndFromA),
            0xe2 => OP::LD(LDType::IndFromA(Indirect::CInd)),
            0xaf => OP::XOR(ArithType::Register(Reg::A)),
            _ => bail!("Invalid opcode: {:02x}", byte),
        };
        Ok(op)
    }

    pub fn from_prefix_byte(byte: u8) -> Result<Self> {
        let op = match byte {
            0x7c => OP::BIT(BitPosition::Seven, BaseTarget::Register(Reg::H)),
            _ => bail!("Invalid opcode: {:02x}", byte),
        };
        Ok(op)
    }
}

pub struct Instruction {
    pub op: OP,
    pub len: u16,
    pub cycles: u64,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} [{}, {}]", self.op, self.len, self.cycles)
    }
}

impl Instruction {
    pub fn from_byte(byte: u8, prefix: bool) -> Result<Self> {
        let op = if !prefix {
            OP::from_byte(byte)?
        } else {
            OP::from_prefix_byte(byte)?
        };
        let (len, cycles) = Instruction::get_len_cycles(&op);
        Ok(Instruction { op, len, cycles })
    }

    fn get_ld_len_cycles(ld_type: &LDType) -> (u16, u64) {
        match ld_type {
            LDType::MoveByte(_, _) => (1, 4),
            LDType::SPFromHL
            | LDType::FromHLInd(_)
            | LDType::ToHLInd(_)
            | LDType::AFromInd(_)
            | LDType::IndFromA(_) => (1, 8),
            LDType::ByteImm(_) => (2, 8),
            LDType::HLFromByteInd
            | LDType::AFromByteInd
            | LDType::ByteIndFromA
            | LDType::HLFromSPi8 => (2, 12),
            LDType::WordImm(_) => (3, 12),
            LDType::AFromWordInd | LDType::WordIndFromA => (3, 16),
            LDType::AddrFromSP => (3, 20),
        }
    }

    fn get_arith_len_cycles(arith_type: &ArithType) -> (u16, u64) {
        match arith_type {
            ArithType::Register(_) => (1, 4),
            ArithType::HLIndirect => (1, 8),
            ArithType::Imm8 => (2, 8),
        }
    }

    fn get_base_len_cycles(base_type: &BaseTarget) -> (u16, u64) {
        match base_type {
            BaseTarget::Register(_) => (1, 4),
            BaseTarget::HLIndirect => (1, 12),
        }
    }

    fn get_len_cycles(op: &OP) -> (u16, u64) {
        match op {
            OP::NOP => (1, 4),
            OP::BIT(_, BaseTarget::Register(_)) | OP::JR(_) => (2, 8),
            OP::BIT(_, BaseTarget::HLIndirect) => (2, 12),
            OP::LD(ld_type) => Instruction::get_ld_len_cycles(ld_type),
            OP::XOR(arith_type) | OP::AND(arith_type) | OP::OR(arith_type) => {
                Instruction::get_arith_len_cycles(arith_type)
            }
            OP::INC(base_type) | OP::DEC(base_type) => Instruction::get_base_len_cycles(base_type),
            OP::INCWord(_) | OP::DECWord(_) => (1, 12),
        }
    }
}
