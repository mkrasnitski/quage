#![allow(dead_code)]
use anyhow::{bail, Result};

use crate::gpu::*;
use crate::instruction::*;

fn signed_offset_u16(x: u16, y: u8) -> u16 {
    (x as i16 + y as i8 as i16) as u16
}

pub struct CPU {
    pc: u16,
    sp: u16,
    cycles: u64,
    registers: Registers,
    bus: MemoryBus,
}

impl CPU {
    pub fn new(bootrom: Vec<u8>) -> Self {
        let mut cpu = CPU {
            pc: 0,
            sp: 0,
            cycles: 0,
            registers: Registers {
                a: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                f: 0,
                h: 0,
                l: 0,
            },
            bus: MemoryBus {
                memory: [0; 0x10000],
                gpu: GPU {},
            },
        };
        cpu.bus.memory[..bootrom.len()].copy_from_slice(&bootrom);
        cpu
    }

    fn state(&self) -> String {
        let r = &self.registers;
        format!(
            "{:04x} {: >2} {:04x} {:04b} [{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}]",
            self.pc,
            self.cycles,
            self.sp,
            r.f >> 4,
            r.a,
            r.b,
            r.c,
            r.d,
            r.e,
            r.h,
            r.l
        )
    }

    pub fn step(&mut self) -> Result<()> {
        let byte = self.bus.read_byte(self.pc);
        let (byte, prefix) = if byte == 0xcb {
            (self.next_byte(), true)
        } else {
            (byte, false)
        };
        let mut instr = Instruction::from_byte(byte, prefix)?;
        let bytes = self
            .bus
            .read_bytes(self.pc, instr.len)
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<String>>()
            .join(" ");
        let state = format!("{} {: >8}", self.state(), bytes);
        self.pc = self.execute(&mut instr)?;
        println!("{} -> {}", state, instr);
        self.cycles += instr.cycles;
        Ok(())
    }

    fn execute(&mut self, instr: &mut Instruction) -> Result<u16> {
        let mut next_pc = self.pc + instr.len;
        match instr.op {
            OP::NOP => (),
            OP::LD(LDType::ByteImm(reg)) => self.write_reg(reg, self.next_byte()),
            OP::LD(LDType::WordImm(word)) => self.write_word(word, self.next_word()),
            OP::LD(LDType::IndFromA(ind)) => self.write_ind(ind, self.read_reg(Reg::A)),
            OP::XOR(arith_type) => match arith_type {
                ArithType::Register(reg) => self.xor(self.read_reg(reg)),
                ArithType::HLIndirect => self.xor(self.read_hl_ind()),
                ArithType::Imm8 => self.xor(self.next_byte()),
            },

            OP::BIT(bit, target) => {
                let res = self.read_base_target(target);
                self.write_flags(FlagState {
                    z: Some((res & (1 << (bit as u8))) == 0),
                    n: Some(false),
                    h: Some(true),
                    c: None,
                });
            }
            OP::JR(jp_type) => {
                let jp_addr = signed_offset_u16(next_pc, self.next_byte());
                next_pc = self.conditional_jump(jp_type, jp_addr, instr);
            }
            _ => bail!("Unimplemented Instruction: {:?}", instr.op),
        };
        Ok(next_pc)
    }

    fn next_byte(&self) -> u8 {
        self.bus.read_byte(self.pc + 1)
    }

    fn next_word(&self) -> u16 {
        self.bus.read_word(self.pc + 1)
    }

    fn conditional_jump(&mut self, jp_type: JPType, addr: u16, instr: &mut Instruction) -> u16 {
        if let Some(addr) = self.branch_addr(jp_type, addr) {
            instr.cycles += 4;
            addr
        } else {
            self.pc + instr.len
        }
    }

    fn branch_addr(&self, jp_type: JPType, addr: u16) -> Option<u16> {
        let condition = match jp_type {
            JPType::Zero => self.read_flag(Flag::Z),
            JPType::NotZero => !self.read_flag(Flag::Z),
            JPType::Carry => self.read_flag(Flag::C),
            JPType::NotCarry => !self.read_flag(Flag::C),
            JPType::Always => true,
        };
        if condition {
            Some(addr)
        } else {
            None
        }
    }

    fn xor(&mut self, y: u8) {
        let val = self.registers.a ^ y;
        self.write_flags(FlagState {
            z: Some(val == 0),
            n: Some(false),
            h: Some(false),
            c: Some(false),
        });
        self.write_reg(Reg::A, val);
    }

    fn read_flag(&self, flag: Flag) -> bool {
        let f = self.registers.f;
        match flag {
            Flag::Z => f & (1 << 7) != 0,
            Flag::N => f & (1 << 6) != 0,
            Flag::H => f & (1 << 5) != 0,
            Flag::C => f & (1 << 4) != 0,
        }
    }

    fn write_flags(&mut self, flags: FlagState) {
        let f = [flags.z, flags.n, flags.h, flags.c];
        for (i, &v) in f.iter().enumerate() {
            if let Some(v) = v {
                let x: u8 = 1 << (7 - i);
                if v {
                    self.registers.f |= x;
                } else {
                    self.registers.f &= !x;
                }
            }
        }
    }

    fn read_reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::A => self.registers.a,
            Reg::B => self.registers.b,
            Reg::C => self.registers.c,
            Reg::D => self.registers.d,
            Reg::E => self.registers.e,
            Reg::H => self.registers.h,
            Reg::L => self.registers.l,
        }
    }

    fn write_reg(&mut self, reg: Reg, val: u8) {
        let rg = match reg {
            Reg::A => &mut self.registers.a,
            Reg::B => &mut self.registers.b,
            Reg::C => &mut self.registers.c,
            Reg::D => &mut self.registers.d,
            Reg::E => &mut self.registers.e,
            Reg::H => &mut self.registers.h,
            Reg::L => &mut self.registers.l,
        };
        *rg = val;
    }

    fn read_word(&self, w: Word) -> u16 {
        match w {
            Word::BC => ((self.registers.b as u16) << 8) | self.registers.c as u16,
            Word::DE => ((self.registers.d as u16) << 8) | self.registers.e as u16,
            Word::HL => ((self.registers.h as u16) << 8) | self.registers.l as u16,
            Word::SP => self.sp,
        }
    }

    fn write_word(&mut self, w: Word, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = val as u8;
        match w {
            Word::SP => self.sp = val,
            Word::BC => {
                self.registers.b = hi;
                self.registers.c = lo;
            }
            Word::DE => {
                self.registers.d = hi;
                self.registers.e = lo;
            }
            Word::HL => {
                self.registers.h = hi;
                self.registers.l = lo;
            }
        }
    }

    fn read_base_target(&self, target: BaseTarget) -> u8 {
        match target {
            BaseTarget::Register(reg) => self.read_reg(reg),
            BaseTarget::HLIndirect => self.read_hl_ind(),
        }
    }

    fn write_base_target(&mut self, target: BaseTarget, val: u8) {
        match target {
            BaseTarget::Register(reg) => self.write_reg(reg, val),
            BaseTarget::HLIndirect => self.write_hl_ind(val),
        }
    }

    fn read_hl_ind(&self) -> u8 {
        self.bus.read_byte(self.read_word(Word::HL))
    }

    fn write_hl_ind(&mut self, val: u8) {
        self.bus.write_byte(self.read_word(Word::HL), val);
    }

    fn read_ind(&mut self, ind: Indirect) -> u8 {
        match ind {
            Indirect::CInd => self.bus.read_byte(0xFF00 + self.read_reg(Reg::C) as u16),
            Indirect::BCInd => self.bus.read_byte(self.read_word(Word::BC)),
            Indirect::DEInd => self.bus.read_byte(self.read_word(Word::DE)),
            Indirect::HLIndPlus => {
                let res = self.bus.read_byte(self.read_word(Word::HL));
                let hl = self.read_word(Word::HL);
                self.write_word(Word::HL, hl + 1);
                res
            }
            Indirect::HLIndMinus => {
                let res = self.bus.read_byte(self.read_word(Word::HL));
                let hl = self.read_word(Word::HL);
                self.write_word(Word::HL, hl - 1);
                res
            }
        }
    }

    fn write_ind(&mut self, ind: Indirect, val: u8) {
        match ind {
            Indirect::CInd => self
                .bus
                .write_byte(0xFF00 + self.read_reg(Reg::C) as u16, val),
            Indirect::BCInd => self.bus.write_byte(self.read_word(Word::BC), val),
            Indirect::DEInd => self.bus.write_byte(self.read_word(Word::DE), val),
            Indirect::HLIndPlus => {
                self.bus.write_byte(self.read_word(Word::HL), val);
                let hl = self.read_word(Word::HL);
                self.write_word(Word::HL, hl + 1);
            }
            Indirect::HLIndMinus => {
                self.bus.write_byte(self.read_word(Word::HL), val);
                let hl = self.read_word(Word::HL);
                self.write_word(Word::HL, hl - 1);
            }
        }
    }
}

pub struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
}

struct MemoryBus {
    memory: [u8; 0x10000],
    gpu: GPU,
}

impl MemoryBus {
    fn read_byte(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val
    }

    fn read_bytes(&self, addr: u16, len: u16) -> &[u8] {
        &self.memory[addr as usize..addr as usize + len as usize]
    }

    fn read_word(&self, addr: u16) -> u16 {
        ((self.memory[addr as usize + 1] as u16) << 8) | (self.memory[addr as usize] as u16)
    }
}
