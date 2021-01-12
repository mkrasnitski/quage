#![allow(dead_code)]
use anyhow::{bail, Result};

use crate::gpu::*;
use crate::instruction::*;

pub struct CPU {
    pc: u16,
    sp: u16,
    prefix: bool,
    registers: Registers,
    bus: MemoryBus,
}

impl CPU {
    pub fn new(bootrom: Vec<u8>) -> Self {
        let mut cpu = CPU {
            pc: 0,
            sp: 0,
            prefix: false,
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
            "{:x} {:x} [{} {} {} {} {} {} {}]",
            self.pc, self.sp, r.a, r.b, r.c, r.d, r.e, r.h, r.l
        )
    }

    pub fn step(&mut self) -> Result<u64> {
        let byte = self.bus.read_byte(self.pc);
        let instr = Instruction::from_byte(byte, self.prefix)?;
        if self.prefix {
            self.prefix = false;
        }
        let (next_pc, cycles) = self.execute(&instr)?;
        println!("{:x} {:?} {}", byte, instr, self.state());
        self.pc = next_pc;
        Ok(cycles)
    }

    fn execute(&mut self, instr: &Instruction) -> Result<(u16, u64)> {
        match instr.op {
            OP::NOP => (),
            OP::LD(LDType::WordImm(word)) => self.set_word(word, self.bus.read_word(self.pc + 1)),
            OP::LD(LDType::IndFromA(ind)) => self.set_ind(ind, self.read_reg(Reg::A)),
            OP::XOR(ArithType::Register(reg)) => {
                self.set_reg(Reg::A, self.read_reg(Reg::A) ^ self.read_reg(reg))
            }
            OP::CB => self.prefix = true,
            _ => bail!("Unimplemented Instruction: {:?}", instr.op),
        };
        Ok((self.pc + instr.len, instr.cycles))
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

    fn set_reg(&mut self, reg: Reg, val: u8) {
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

    fn set_word(&mut self, w: Word, val: u16) {
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

    fn read_ind(&mut self, ind: Indirect) -> u8 {
        match ind {
            Indirect::CInd => self.bus.read_byte(0xFF00 + self.registers.c as u16),
            Indirect::BCInd => self.bus.read_byte(self.read_word(Word::BC)),
            Indirect::DEInd => self.bus.read_byte(self.read_word(Word::DE)),
            Indirect::HLIndPlus => {
                let res = self.bus.read_byte(self.read_word(Word::HL));
                let hl = self.read_word(Word::HL);
                self.set_word(Word::HL, hl + 1);
                res
            }
            Indirect::HLIndMinus => {
                let res = self.bus.read_byte(self.read_word(Word::HL));
                let hl = self.read_word(Word::HL);
                self.set_word(Word::HL, hl - 1);
                res
            }
        }
    }

    fn set_ind(&mut self, ind: Indirect, val: u8) {
        match ind {
            Indirect::CInd => self.bus.set_byte(0xFF00 + self.registers.c as u16, val),
            Indirect::BCInd => self.bus.set_byte(self.read_word(Word::BC), val),
            Indirect::DEInd => self.bus.set_byte(self.read_word(Word::DE), val),
            Indirect::HLIndPlus => {
                self.bus.set_byte(self.read_word(Word::HL), val);
                let hl = self.read_word(Word::HL);
                self.set_word(Word::HL, hl + 1);
            }
            Indirect::HLIndMinus => {
                self.bus.set_byte(self.read_word(Word::HL), val);
                let hl = self.read_word(Word::HL);
                self.set_word(Word::HL, hl - 1);
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

    fn set_byte(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val
    }

    fn read_word(&self, addr: u16) -> u16 {
        ((self.memory[addr as usize + 1] as u16) << 8) | (self.memory[addr as usize] as u16)
    }
}
