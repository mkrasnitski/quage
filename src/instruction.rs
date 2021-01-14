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

#[derive(Debug, Copy, Clone)]
pub enum R8 {
    Register(Reg),
    HLIndirect,
}

#[derive(Debug)]
pub enum LDType {
    // 92 total LD instrs
    MoveByte(R8, R8),   // (63) LD r, r
    AFromInd(Indirect), //  (5) LD A, (XX)
    IndFromA(Indirect), //  (5) LD (xx), A
    ByteImm(R8),        //  (8) LD r, u8
    WordImm(Word),      //  (4) LD ww, u16
    AFromByteInd,       //  (1) LD A, (FF00+u8)
    ByteIndFromA,       //  (1) LD (FF00+u8), A
    AFromWordInd,       //  (1) LD A, (u16)
    WordIndFromA,       //  (1) LD (u16), A
    SPFromHL,           //  (1) LD SP, HL
    HLFromSPi8,         //  (1) LD HL, SP + i8
    AddrFromSP,         //  (1) LD (u16), SP
}

#[derive(Debug, Copy, Clone)]
pub enum ArithType {
    R8(R8),
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
    R8(R8),
    Word(Word),
}

#[derive(Debug)]
pub enum OP {
    LD(LDType),

    AND(ArithType),
    OR(ArithType),
    XOR(ArithType),

    INC(IncDecTarget),
    DEC(IncDecTarget),

    BIT(BitPosition, R8),

    JR(JPType),
    CALL(JPType),

    NOP,
}

impl OP {
    pub fn from_byte(byte: u8) -> Result<Self> {
        let op = match byte {
            0x00 => OP::NOP,
            0x0c => OP::INC(IncDecTarget::R8(R8::Register(Reg::C))),
            0x0e => OP::LD(LDType::ByteImm(R8::Register(Reg::C))),
            0x11 => OP::LD(LDType::WordImm(Word::DE)),
            0x1a => OP::LD(LDType::AFromInd(Indirect::DEInd)),
            0x20 => OP::JR(JPType::NotZero),
            0x21 => OP::LD(LDType::WordImm(Word::HL)),
            0x31 => OP::LD(LDType::WordImm(Word::SP)),
            0x3e => OP::LD(LDType::ByteImm(R8::Register(Reg::A))),
            0x32 => OP::LD(LDType::IndFromA(Indirect::HLIndMinus)),
            0x40..=0x75 | 0x77..=0x7f => OP::ld_r8_from_byte(byte)?,
            0xcd => OP::CALL(JPType::Always),
            0xe0 => OP::LD(LDType::ByteIndFromA),
            0xe2 => OP::LD(LDType::IndFromA(Indirect::CInd)),
            0xaf => OP::XOR(ArithType::R8(R8::Register(Reg::A))),
            _ => bail!("Invalid opcode: {:02x}", byte),
        };
        Ok(op)
    }

    fn ld_r8_from_byte(byte: u8) -> Result<OP> {
        if byte < 0x40 || byte > 0x7f || byte == 0x76 {
            bail!("Invalid LD R8, R8 byte: {}", byte);
        }
        let src = OP::decode_r8(byte & 0b111)?;
        let dest = OP::decode_r8((byte & 0b111000) >> 3)?;
        Ok(OP::LD(LDType::MoveByte(dest, src)))
    }

    fn decode_r8(val: u8) -> Result<R8> {
        let ret = match val {
            0 => R8::Register(Reg::B),
            1 => R8::Register(Reg::C),
            2 => R8::Register(Reg::D),
            3 => R8::Register(Reg::E),
            4 => R8::Register(Reg::H),
            5 => R8::Register(Reg::L),
            6 => R8::HLIndirect,
            7 => R8::Register(Reg::A),
            _ => bail!("Invalid R8 val: {}", val),
        };
        Ok(ret)
    }

    pub fn from_prefix_byte(byte: u8) -> Result<Self> {
        let op = match byte {
            0x7c => OP::BIT(BitPosition::Seven, R8::Register(Reg::H)),
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
            LDType::MoveByte(R8::Register(_), R8::Register(_)) => (1, 4),
            LDType::SPFromHL
            | LDType::MoveByte(R8::HLIndirect, _)
            | LDType::MoveByte(_, R8::HLIndirect)
            | LDType::AFromInd(_)
            | LDType::IndFromA(_) => (1, 8),
            LDType::ByteImm(R8::Register(_)) => (2, 8),
            LDType::ByteImm(R8::HLIndirect)
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
            ArithType::R8(R8::Register(_)) => (1, 4),
            ArithType::R8(R8::HLIndirect) => (1, 8),
            ArithType::Imm8 => (2, 8),
        }
    }

    fn get_r8_len_cycles(r8: &R8) -> (u16, u64) {
        match r8 {
            R8::Register(_) => (1, 4),
            R8::HLIndirect => (1, 12),
        }
    }

    fn get_bit_len_cycles(r8: &R8) -> (u16, u64) {
        match r8 {
            R8::Register(_) => (2, 8),
            R8::HLIndirect => (2, 12),
        }
    }

    fn get_inc_dec_len_cycles(id_target: &IncDecTarget) -> (u16, u64) {
        match id_target {
            IncDecTarget::R8(r8) => Instruction::get_r8_len_cycles(r8),
            IncDecTarget::Word(_) => (1, 12),
        }
    }

    fn get_len_cycles(op: &OP) -> (u16, u64) {
        match op {
            OP::NOP => (1, 4),
            OP::JR(_) => (2, 8),
            OP::CALL(_) => (3, 12),
            OP::BIT(_, r8) => Instruction::get_bit_len_cycles(r8),
            OP::LD(ld_type) => Instruction::get_ld_len_cycles(ld_type),
            OP::XOR(arith_type) | OP::AND(arith_type) | OP::OR(arith_type) => {
                Instruction::get_arith_len_cycles(arith_type)
            }
            OP::INC(id_target) | OP::DEC(id_target) => {
                Instruction::get_inc_dec_len_cycles(id_target)
            }
        }
    }
}
