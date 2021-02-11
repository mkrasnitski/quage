use anyhow::{bail, Result};
use enum_primitive_derive::Primitive;
use num_traits::FromPrimitive;
use std::fmt;

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
    pub fn from_byte(byte: u8) -> Result<Self> {
        let b1 = (byte >> 3) & 0b111;
        let b2 = byte & 0b111;
        let b3 = b1 >> 1;
        let b4 = b1 & 0b11;
        let w = Word::from_u8(b3).unwrap();
        let ind = Indirect::from_u8(b3).unwrap();
        let r81 = R8::from_u8(b1).unwrap();
        let r82 = R8::from_u8(b2).unwrap();
        let branch = BranchCondition::from_u8(b4).unwrap();
        let push_pop = PushPopTarget::from_u8(b3).unwrap();
        Ok(match byte {
            0x01 | 0x11 | 0x21 | 0x31 => OP::LD(LDType::WordImm(w)),
            0x02 | 0x12 | 0x22 | 0x32 => OP::LD(LDType::IndFromA(ind)),
            0x03 | 0x13 | 0x23 | 0x33 => OP::INC(IncDecTarget::Word(w)),
            0x04 | 0x14 | 0x24 | 0x34 | 0x0c | 0x1c | 0x2c | 0x3c => OP::INC(IncDecTarget::R8(r81)),
            0x05 | 0x15 | 0x25 | 0x35 | 0x0d | 0x1d | 0x2d | 0x3d => OP::DEC(IncDecTarget::R8(r81)),
            0x06 | 0x16 | 0x26 | 0x36 | 0x0e | 0x1e | 0x2e | 0x3e => OP::LD(LDType::ByteImm(r81)),
            0x09 | 0x19 | 0x29 | 0x39 => OP::ADDHL(w),
            0x0a | 0x1a | 0x2a | 0x3a => OP::LD(LDType::AFromInd(ind)),
            0x0b | 0x1b | 0x2b | 0x3b => OP::DEC(IncDecTarget::Word(w)),
            0x20 | 0x30 | 0x28 | 0x38 => OP::JR(branch),
            0x40..=0x75 | 0x77..=0x7f => OP::LD(LDType::MoveByte(r81, r82)),
            0x80..=0xbf => OP::decode_alu_op(b1, ALUType::R8(r82))?,
            0xc0 | 0xd0 | 0xc8 | 0xd8 => OP::RET(branch),
            0xc1 | 0xd1 | 0xe1 | 0xf1 => OP::POP(push_pop),
            0xc2 | 0xd2 | 0xca | 0xda => OP::JP(branch),
            0xc4 | 0xd4 | 0xcc | 0xdc => OP::CALL(branch),
            0xc5 | 0xd5 | 0xe5 | 0xf5 => OP::PUSH(push_pop),
            0xc6 | 0xd6 | 0xe6 | 0xf6 | 0xce | 0xde | 0xee | 0xfe => {
                OP::decode_alu_op(b1, ALUType::Imm8)?
            }
            0xc7 | 0xd7 | 0xe7 | 0xf7 | 0xcf | 0xdf | 0xef | 0xff => {
                let addr = b2 << 3;
                match addr {
                    0x00 | 0x08 | 0x10 | 0x18 | 0x20 | 0x28 | 0x30 | 0x38 => OP::RST(addr as u16),
                    _ => bail!("Invalid RST addr: {:#02x}", addr),
                }
            }

            0x00 => OP::NOP,
            0x07 => OP::RLCA,
            0x08 => OP::LD(LDType::AddrFromSP),
            0x10 => OP::STOP,
            0x0f => OP::RRCA,
            0x17 => OP::RLA,
            0x1f => OP::RRA,
            0x18 => OP::JR(BranchCondition::Always),
            0x27 => OP::DAA,
            0x2f => OP::CPL,
            0x37 => OP::SCF,
            0x3f => OP::CCF,
            0x76 => OP::HALT,
            0xc3 => OP::JP(BranchCondition::Always),
            0xc9 => OP::RET(BranchCondition::Always),
            0xcd => OP::CALL(BranchCondition::Always),
            0xd9 => OP::RETI,
            0xe0 => OP::LD(LDType::ByteIndFromA),
            0xe2 => OP::LD(LDType::IndFromA(Indirect::CInd)),
            0xe8 => OP::ADDSPi8,
            0xe9 => OP::JPHL,
            0xea => OP::LD(LDType::AddrFromA),
            0xf0 => OP::LD(LDType::AFromByteInd),
            0xf2 => OP::LD(LDType::AFromInd(Indirect::CInd)),
            0xf3 => OP::DI,
            0xf8 => OP::LD(LDType::HLFromSPi8),
            0xf9 => OP::LD(LDType::SPFromHL),
            0xfa => OP::LD(LDType::AFromAddr),
            0xfb => OP::EI,

            0xcb => bail!("Somehow didn't catch prefix."),
            0xd3 | 0xdb | 0xdd | 0xe3 | 0xe4 | 0xeb | 0xec | 0xed | 0xf4 | 0xfc | 0xfd => {
                bail!("Invalid opcode: {:02x}", byte)
            }
        })
    }

    pub fn from_prefix_byte(byte: u8) -> Self {
        let bit = (byte >> 3) & 0b111;
        let bit = BitPosition::from_u8(bit).unwrap();
        let r8 = R8::from_u8(byte & 0b111).unwrap();
        match byte {
            0x00..=0x07 => OP::RLC(r8),
            0x08..=0x0f => OP::RRC(r8),
            0x10..=0x17 => OP::RL(r8),
            0x18..=0x1f => OP::RR(r8),
            0x20..=0x27 => OP::SLA(r8),
            0x28..=0x2f => OP::SRA(r8),
            0x30..=0x37 => OP::SWAP(r8),
            0x38..=0x3f => OP::SRL(r8),
            0x40..=0x7f => OP::BIT(bit, r8),
            0x80..=0xbf => OP::RES(bit, r8),
            0xc0..=0xff => OP::SET(bit, r8),
        }
    }

    fn decode_alu_op(op: u8, arg: ALUType) -> Result<OP> {
        Ok(match op {
            0 => OP::ADD(arg),
            1 => OP::ADC(arg),
            2 => OP::SUB(arg),
            3 => OP::SBC(arg),
            4 => OP::AND(arg),
            5 => OP::XOR(arg),
            6 => OP::OR(arg),
            7 => OP::CP(arg),
            _ => bail!("Invalid ALU op: {}", op),
        })
    }
}

#[derive(Debug)]
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
            OP::from_prefix_byte(byte)
        };
        let (len, cycles) = Instruction::get_len_cycles(&op);
        Ok(Instruction { op, len, cycles })
    }

    fn get_len_cycles(op: &OP) -> (u16, u64) {
        match op {
            OP::NOP
            | OP::HALT
            | OP::DI
            | OP::EI
            | OP::RLA
            | OP::RRA
            | OP::RLCA
            | OP::RRCA
            | OP::JPHL
            | OP::DAA
            | OP::CPL
            | OP::SCF
            | OP::CCF => (1, 4),
            OP::ADDHL(_) => (1, 8),
            OP::POP(_) => (1, 12),
            OP::PUSH(_) | OP::RST(_) | OP::RETI => (1, 16),
            OP::STOP => (2, 4),
            OP::JR(_) => (2, 8),
            OP::ADDSPi8 => (2, 16),
            OP::JP(_) | OP::CALL(_) => (3, 12),
            OP::RET(condition) => match condition {
                BranchCondition::Always => (1, 4),
                _ => (1, 8),
            },
            OP::BIT(_, r8) => match r8 {
                R8::HLInd => (2, 12),
                _ => (2, 8),
            },
            OP::RES(_, r8)
            | OP::SET(_, r8)
            | OP::RL(r8)
            | OP::RR(r8)
            | OP::RLC(r8)
            | OP::RRC(r8)
            | OP::SLA(r8)
            | OP::SRA(r8)
            | OP::SRL(r8)
            | OP::SWAP(r8) => match r8 {
                R8::HLInd => (2, 16),
                _ => (2, 8),
            },
            OP::XOR(alu)
            | OP::AND(alu)
            | OP::OR(alu)
            | OP::ADD(alu)
            | OP::ADC(alu)
            | OP::SUB(alu)
            | OP::SBC(alu)
            | OP::CP(alu) => match alu {
                ALUType::R8(r8) => match r8 {
                    R8::HLInd => (1, 8),
                    _ => (1, 4),
                },
                ALUType::Imm8 => (2, 8),
            },
            OP::INC(id_target) | OP::DEC(id_target) => match id_target {
                IncDecTarget::Word(_) => (1, 8),
                IncDecTarget::R8(R8::HLInd) => (1, 12),
                IncDecTarget::R8(_) => (1, 4),
            },
            OP::LD(ld_type) => match ld_type {
                LDType::MoveByte(dest, src) => match (dest, src) {
                    (R8::HLInd, _) | (_, R8::HLInd) => (1, 8),
                    (_, _) => (1, 4),
                },
                LDType::SPFromHL | LDType::AFromInd(_) | LDType::IndFromA(_) => (1, 8),
                LDType::ByteImm(r8) => match r8 {
                    R8::HLInd => (2, 12),
                    _ => (2, 8),
                },
                LDType::AFromByteInd | LDType::ByteIndFromA | LDType::HLFromSPi8 => (2, 12),
                LDType::WordImm(_) => (3, 12),
                LDType::AFromAddr | LDType::AddrFromA => (3, 16),
                LDType::AddrFromSP => (3, 20),
            },
        }
    }
}
