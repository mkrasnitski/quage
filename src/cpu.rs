#![allow(dead_code)]
use anyhow::{bail, Result};

use crate::flags::*;
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
                f: Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: false,
                },
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
            u8::from(r.f) >> 4,
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
            OP::LD(LDType::ByteImm(R8::Register(reg))) => self.write_reg(reg, self.next_byte()),
            OP::LD(LDType::WordImm(word)) => self.write_word(word, self.next_word()),
            OP::LD(LDType::IndFromA(ind)) => self.write_ind(ind, self.read_reg(Reg::A)),
            OP::LD(LDType::AFromInd(ind)) => {
                let res = self.read_ind(ind);
                self.write_reg(Reg::A, res);
            }
            OP::LD(LDType::MoveByte(dest, src)) => self.write_r8(dest, self.read_r8(src)),
            OP::LD(LDType::ByteIndFromA) => {
                self.write_mem_addr(0xFF00 + self.next_byte() as u16, self.read_reg(Reg::A));
            }

            OP::AND(arith_type) => {
                let val = self.registers.a & self.read_arith_type(arith_type);
                self.registers.f.z = val == 0;
                self.registers.f.n = false;
                self.registers.f.h = true;
                self.registers.f.c = false;
                self.write_reg(Reg::A, val);
            }
            OP::OR(arith_type) => {
                let val = self.registers.a | self.read_arith_type(arith_type);
                self.registers.f.z = val == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = false;
                self.write_reg(Reg::A, val);
            }
            OP::XOR(arith_type) => {
                let val = self.registers.a ^ self.read_arith_type(arith_type);
                self.registers.f.z = val == 0;
                self.registers.f.n = false;
                self.registers.f.h = false;
                self.registers.f.c = false;
                self.write_reg(Reg::A, val);
            }
            OP::INC(IncDecTarget::R8(r8)) => {
                let (val, h, _) = self.read_r8(r8).overflowing_hc_add(1);
                self.registers.f.z = val == 0;
                self.registers.f.n = false;
                self.registers.f.h = h;
                self.write_r8(r8, val);
            }

            OP::BIT(bit, target) => {
                let res = self.read_r8(target);
                self.registers.f.z = (res & (1 << (bit as u8))) == 0;
                self.registers.f.n = false;
                self.registers.f.h = true;
            }

            OP::JR(jp_type) => {
                if self.check_branch_condition(jp_type) {
                    next_pc = signed_offset_u16(next_pc, self.next_byte());
                    instr.cycles += 4;
                }
            }
            OP::CALL(jp_type) => {
                if self.check_branch_condition(jp_type) {
                    self.push_word(next_pc);
                    next_pc = self.next_word();
                    instr.cycles += 12;
                }
            }

            OP::NOP => (),
            _ => bail!("Unimplemented Instruction: {:?}", instr.op),
        };
        Ok(next_pc)
    }

    fn pop(&mut self) -> u8 {
        let res = self.bus.read_byte(self.sp);
        self.sp += 1;
        res
    }

    fn push(&mut self, val: u8) {
        self.sp -= 1;
        self.bus.write_byte(self.sp, val);
    }

    fn pop_word(&mut self) -> u16 {
        let w1 = self.pop() as u16;
        let w2 = self.pop() as u16;
        (w2 << 8) | w1
    }

    fn push_word(&mut self, val: u16) {
        self.push((val >> 8) as u8);
        self.push(val as u8);
    }

    fn next_byte(&self) -> u8 {
        self.bus.read_byte(self.pc + 1)
    }

    fn next_word(&self) -> u16 {
        self.bus.read_word(self.pc + 1)
    }

    fn read_mem_addr(&self, addr: u16) -> u8 {
        self.bus.read_byte(addr)
    }

    fn write_mem_addr(&mut self, addr: u16, val: u8) {
        self.bus.write_byte(addr, val)
    }

    fn check_branch_condition(&self, jp_type: JPType) -> bool {
        match jp_type {
            JPType::Zero => self.read_flag(Flag::Z),
            JPType::NotZero => !self.read_flag(Flag::Z),
            JPType::Carry => self.read_flag(Flag::C),
            JPType::NotCarry => !self.read_flag(Flag::C),
            JPType::Always => true,
        }
    }

    fn read_flag(&self, flag: Flag) -> bool {
        let f = &self.registers.f;
        match flag {
            Flag::Z => f.z,
            Flag::N => f.n,
            Flag::H => f.h,
            Flag::C => f.c,
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

    fn read_arith_type(&self, arith_type: ArithType) -> u8 {
        match arith_type {
            ArithType::R8(R8::Register(reg)) => self.read_reg(reg),
            ArithType::R8(R8::HLIndirect) => self.read_hl_ind(),
            ArithType::Imm8 => self.next_byte(),
        }
    }

    fn read_r8(&self, target: R8) -> u8 {
        match target {
            R8::Register(reg) => self.read_reg(reg),
            R8::HLIndirect => self.read_hl_ind(),
        }
    }

    fn write_r8(&mut self, target: R8, val: u8) {
        match target {
            R8::Register(reg) => self.write_reg(reg, val),
            R8::HLIndirect => self.write_hl_ind(val),
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
    f: Flags,
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
