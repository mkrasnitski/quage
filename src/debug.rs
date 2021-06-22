use crate::instruction::*;
use std::fmt;

pub trait InstructionDisplay {
    fn to_instr_string(&self, bytes: &[u8]) -> String;
}

fn word_arg(bytes: &[u8]) -> u16 {
    ((bytes[2] as u16) << 8) | (bytes[1] as u16)
}

impl fmt::Display for R8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                R8::B => "B",
                R8::C => "C",
                R8::D => "D",
                R8::E => "E",
                R8::H => "H",
                R8::L => "L",
                R8::HLInd => "(HL)",
                R8::A => "A",
            }
        )
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for Indirect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Indirect::BCInd => "(BC)",
                Indirect::DEInd => "(DE)",
                Indirect::HLIndPlus => "(HL+)",
                Indirect::HLIndMinus => "(HL-)",
                Indirect::CInd => "(FF00 + C)",
            }
        )
    }
}

impl fmt::Display for PushPopTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for BranchCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BranchCondition::NotZero => " NZ,",
                BranchCondition::Zero => " Z,",
                BranchCondition::NotCarry => " NC,",
                BranchCondition::Carry => " C,",
                BranchCondition::Always => "",
            }
        )
    }
}

impl fmt::Display for BitPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BitPosition::Zero => "0",
                BitPosition::One => "1",
                BitPosition::Two => "2",
                BitPosition::Three => "3",
                BitPosition::Four => "4",
                BitPosition::Five => "5",
                BitPosition::Six => "6",
                BitPosition::Seven => "7",
            }
        )
    }
}

impl InstructionDisplay for LDType {
    fn to_instr_string(&self, bytes: &[u8]) -> String {
        match self {
            LDType::ByteImm(r8) => format!("{}, {:02x}", r8, bytes[1]),
            LDType::WordImm(word) => format!("{}, {:04x}", word, word_arg(&bytes)),
            LDType::MoveByte(dest, src) => format!("{}, {}", dest, src),
            LDType::IndFromA(ind) => format!("{}, A", ind),
            LDType::AFromInd(ind) => format!("A, {}", ind),
            LDType::ByteIndFromA => format!("({:04X}), A", 0xFF00 + bytes[1] as u16),
            LDType::AFromByteInd => format!("A, ({:04X})", 0xFF00 + bytes[1] as u16),
            LDType::AFromAddr => format!("A, ({:04X})", word_arg(&bytes)),
            LDType::AddrFromA => format!("({:04X}), A", word_arg(&bytes)),
            LDType::SPFromHL => "SP, HL".to_string(),
            LDType::HLFromSPi8 => format!("HL, SP + {:02x}", bytes[1]),
            LDType::AddrFromSP => format!("({:04X}), SP", word_arg(&bytes)),
        }
    }
}

impl InstructionDisplay for ALUType {
    fn to_instr_string(&self, bytes: &[u8]) -> String {
        match self {
            ALUType::R8(r8) => format!("{}", r8),
            ALUType::Imm8 => format!("{:02x}", bytes[1]),
        }
    }
}

impl fmt::Display for IncDecTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IncDecTarget::R8(r8) => write!(f, "{}", r8),
            IncDecTarget::Word(word) => write!(f, "{}", word),
        }
    }
}

impl InstructionDisplay for OP {
    fn to_instr_string(&self, bytes: &[u8]) -> String {
        match self {
            OP::NOP
            | OP::STOP
            | OP::HALT
            | OP::EI
            | OP::DI
            | OP::DAA
            | OP::CPL
            | OP::SCF
            | OP::CCF
            | OP::RLA
            | OP::RRA
            | OP::RLCA
            | OP::RRCA
            | OP::ADDSPi8
            | OP::JPHL
            | OP::RETI => format!("{:?}", self),

            OP::LD(ld_type) => format!("LD {}", ld_type.to_instr_string(&bytes)),

            OP::AND(alu_type) => format!("AND A, {}", alu_type.to_instr_string(&bytes)),
            OP::OR(alu_type) => format!("OR A, {}", alu_type.to_instr_string(&bytes)),
            OP::XOR(alu_type) => format!("XOR A, {}", alu_type.to_instr_string(&bytes)),
            OP::ADD(alu_type) => format!("ADD A, {}", alu_type.to_instr_string(&bytes)),
            OP::ADC(alu_type) => format!("ADC A, {}", alu_type.to_instr_string(&bytes)),
            OP::ADDHL(word) => format!("ADD HL, {}", word),
            OP::SUB(alu_type) => format!("SUB A, {}", alu_type.to_instr_string(&bytes)),
            OP::SBC(alu_type) => format!("SBC A, {}", alu_type.to_instr_string(&bytes)),
            OP::CP(alu_type) => format!("CP A, {}", alu_type.to_instr_string(&bytes)),

            OP::INC(inc_dec) => format!("INC {}", inc_dec),
            OP::DEC(inc_dec) => format!("DEC {}", inc_dec),

            OP::BIT(bit, r8) => format!("BIT {}, {}", bit, r8),
            OP::RES(bit, r8) => format!("RES {}, {}", bit, r8),
            OP::SET(bit, r8) => format!("SET {}, {}", bit, r8),

            OP::RL(r8) => format!("RL {}", r8),
            OP::RR(r8) => format!("RR {}", r8),
            OP::RLC(r8) => format!("RLC {}", r8),
            OP::RRC(r8) => format!("RRC {}", r8),
            OP::SLA(r8) => format!("SLA {}", r8),
            OP::SRA(r8) => format!("SRA {}", r8),
            OP::SRL(r8) => format!("SRL {}", r8),
            OP::SWAP(r8) => format!("SWAP {}", r8),

            OP::PUSH(push_pop_target) => format!("PUSH {}", push_pop_target),
            OP::POP(push_pop_target) => format!("POP {}", push_pop_target),

            OP::RST(v) => format!("RST {:#02x}", v),
            OP::JR(condition) => format!("JR{} {:02x}", condition, bytes[1]),
            OP::JP(condition) => format!("JR{} {:04X}", condition, word_arg(&bytes)),
            OP::CALL(condition) => format!("CALL{} {:04X}", condition, word_arg(&bytes)),
            OP::RET(condition) => {
                let mut c_str = condition.to_string();
                c_str.pop();
                format!("RET{}", c_str)
            }
        }
    }
}
