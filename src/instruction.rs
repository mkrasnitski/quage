#![allow(dead_code)]
use anyhow::{bail, Result};

#[derive(Debug)]
pub enum OP {
    NOP,
    LD(LDType),
    XOR(ArithType),
    CB,
    BIT(BitPosition, PrefixTarget),
}

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

#[derive(Debug)]
pub enum ArithType {
    Register(Reg),
    HLIndirect,
    Imm8,
}

#[derive(Debug)]
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
pub enum PrefixTarget {
    Register(Reg),
    HLInd,
}

impl OP {
    pub fn from_byte(byte: u8) -> Result<Self> {
        let op = match byte {
            0x00 => OP::NOP,
            0x21 => OP::LD(LDType::WordImm(Word::HL)),
            0x31 => OP::LD(LDType::WordImm(Word::SP)),
            0x32 => OP::LD(LDType::IndFromA(Indirect::HLIndMinus)),
            0xaf => OP::XOR(ArithType::Register(Reg::A)),
            0xcb => OP::CB,
            _ => bail!("Invalid opcode: {:#x}", byte),
        };
        Ok(op)
    }

    pub fn from_prefix_byte(byte: u8) -> Result<Self> {
        let op = match byte {
            0x7c => OP::BIT(BitPosition::Seven, PrefixTarget::Register(Reg::H)),
            _ => bail!("Invalid opcode: {:#x}", byte),
        };
        Ok(op)
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub op: OP,
    pub len: u16,
    pub cycles: u64,
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

    fn get_len_cycles(op: &OP) -> (u16, u64) {
        match op {
            OP::NOP | OP::CB => (1, 4),
            OP::BIT(_, _) => (2, 8),
            OP::LD(ld_type) => Instruction::get_ld_len_cycles(ld_type),
            OP::XOR(arith_type) => Instruction::get_arith_len_cycles(arith_type),
        }
    }
}
