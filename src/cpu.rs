#![allow(dead_code, non_snake_case)]
use anyhow::{bail, Result};

use crate::flags::*;
use crate::gpu::*;
use crate::instruction::*;

use std::io::Write;
const DEBUG: bool = false;

fn signed_offset_u16(x: u16, y: u8) -> u16 {
    (x as i16 + y as i8 as i16) as u16
}

pub struct CPU {
    pc: u16,
    sp: u16,
    cycles: u64,
    queue_ime: bool,
    ime: bool,
    halted: bool,
    registers: Registers,
    bus: MemoryBus,
}

impl CPU {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>, skip_bootrom: bool) -> Self {
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
            queue_ime: false,
            ime: false,
            halted: false,
            bus: MemoryBus {
                memory: [0; 0x10000],
                bootrom: [0; 0x100],
                gpu: GPU {},
            },
        };
        cpu.bus.bootrom.copy_from_slice(&bootrom);
        cpu.bus.memory[..cartridge.len()].copy_from_slice(&cartridge);
        if skip_bootrom {
            cpu.registers.a = 0x01;
            cpu.registers.c = 0x13;
            cpu.registers.e = 0xd8;
            cpu.registers.h = 0x01;
            cpu.registers.l = 0x4d;
            cpu.registers.f = Flags::from(0xb0);
            cpu.sp = 0xfffe;
            cpu.bus.memory[0xff10] = 0x80;
            cpu.bus.memory[0xff11] = 0xbf;
            cpu.bus.memory[0xff12] = 0xf3;
            cpu.bus.memory[0xff14] = 0xbf;
            cpu.bus.memory[0xff16] = 0x3f;
            cpu.bus.memory[0xff19] = 0xbf;
            cpu.bus.memory[0xff1a] = 0x7f;
            cpu.bus.memory[0xff1b] = 0xff;
            cpu.bus.memory[0xff1c] = 0x9f;
            cpu.bus.memory[0xff1e] = 0xbf;
            cpu.bus.memory[0xff20] = 0xff;
            cpu.bus.memory[0xff23] = 0xbf;
            cpu.bus.memory[0xff24] = 0x77;
            cpu.bus.memory[0xff25] = 0xf3;
            cpu.bus.memory[0xff26] = 0xf1;
            cpu.bus.memory[0xff40] = 0x91;
            cpu.bus.memory[0xff47] = 0xfc;
            cpu.bus.memory[0xff48] = 0xff;
            cpu.bus.memory[0xff49] = 0xff;
            cpu.bus.memory[0xff50] = 0x01;
            cpu.pc = 0x100;
        }
        cpu
    }

    fn state(&self) -> String {
        let r = &self.registers;
        format!(
            "{:04x} {: >2} {:04x} {} {:04b} [{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}]",
            self.pc,
            self.cycles,
            self.sp,
            self.ime,
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
        self.check_interrupts();
        let cycles_passed = if !self.halted {
            let byte = self.bus.read_byte(self.pc);
            let (byte, prefix) = if byte == 0xcb {
                (self.next_byte(), true)
            } else {
                (byte, false)
            };
            let mut instr = Instruction::from_byte(byte, prefix)?;
            let state = format!(
                "{} {: >8}",
                self.state(),
                self.bus
                    .read_bytes(self.pc, instr.len)
                    .iter()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<String>>()
                    .join(" ")
            );
            self.pc = self.execute(&mut instr)?;
            if DEBUG {
                println!("{} -> {}", state, instr);
            }
            if self.bus.read_byte(0xFF02) == 0x81 {
                let c = self.bus.read_byte(0xFF01) as char;
                self.bus.write_byte(0xFF02, 0x00);
                print!("{}", c);
                std::io::stdout().flush().unwrap();
            }
            instr.cycles
        } else {
            4
        };
        self.increment_timers(cycles_passed);
        self.cycles += cycles_passed;
        Ok(())
    }

    fn check_interrupts(&mut self) {
        let IE = self.bus.read_byte(0xFFFF);
        let IF = self.bus.read_byte(0xFF0F);
        for i in 0..5 {
            let mask = 1u8 << i;
            if IE & IF & mask != 0 {
                self.halted = false;
                if self.ime {
                    self.bus.write_byte(0xFF0F, IF ^ mask);
                    self.push_word(self.pc);
                    self.pc = (i << 3) + 0x40;
                    break;
                }
            }
        }
        if self.queue_ime {
            self.ime = true;
            self.queue_ime = false;
        }
    }

    fn increment_timers(&mut self, cycles_passed: u64) {
        let total_cycles = self.cycles + cycles_passed;
        if total_cycles / 256 > self.cycles / 256 {
            self.bus
                .write_byte(0xFF04, self.bus.read_byte(0xFF04).wrapping_add(1));
        }
        let TAC = self.bus.read_byte(0xFF07);
        if TAC & 0b100 != 0 {
            let clock = match TAC & 0b11 {
                0 => 1024,
                1 => 16,
                2 => 64,
                3 => 256,
                _ => panic!(""),
            };

            if total_cycles / clock > self.cycles / clock {
                let (TIMA, c) = self.bus.read_byte(0xFF05).overflowing_add(1);
                if c {
                    self.bus.write_byte(0xFF05, self.bus.read_byte(0xFF06));
                    self.bus
                        .write_byte(0xFF0F, self.bus.read_byte(0xFF0F) | 0b100);
                } else {
                    self.bus.write_byte(0xFF05, TIMA);
                }
            }
        }
    }

    fn execute(&mut self, instr: &mut Instruction) -> Result<u16> {
        let mut next_pc = self.pc + instr.len;
        match instr.op {
            OP::NOP => (),
            OP::HALT | OP::STOP => self.halted = true,
            OP::DI => self.ime = false,
            OP::EI => self.queue_ime = true,

            OP::LD(ld_type) => match ld_type {
                LDType::ByteImm(r8) => self.write_r8(r8, self.next_byte()),
                LDType::WordImm(word) => self.write_word(word, self.next_word()),
                LDType::IndFromA(ind) => self.write_ind(ind, self.read_r8(R8::A)),
                LDType::AFromInd(ind) => {
                    let res = self.read_ind(ind);
                    self.write_r8(R8::A, res);
                }
                LDType::MoveByte(dest, src) => self.write_r8(dest, self.read_r8(src)),
                LDType::ByteIndFromA => self
                    .bus
                    .write_byte(0xFF00 + self.next_byte() as u16, self.read_r8(R8::A)),
                LDType::AFromByteInd => {
                    self.write_r8(R8::A, self.bus.read_byte(0xFF00 + self.next_byte() as u16))
                }
                LDType::AddrFromA => self.bus.write_byte(self.next_word(), self.read_r8(R8::A)),
                LDType::AFromAddr => self.write_r8(R8::A, self.bus.read_byte(self.next_word())),
                LDType::SPFromHL => self.write_word(Word::SP, self.read_word(Word::HL)),
                LDType::AddrFromSP => self
                    .bus
                    .write_word(self.next_word(), self.read_word(Word::SP)),
                LDType::HLFromSPi8 => {
                    let (val, h, c) = self.sp.overflowing_hc_add_i8(self.next_byte() as i8);
                    self.set_flags(false, false, h, c);
                    self.write_word(Word::HL, val);
                }
            },

            OP::AND(alu_type) => {
                let val = self.registers.a & self.read_alu(alu_type);
                self.set_flags(val == 0, false, true, false);
                self.write_r8(R8::A, val);
            }
            OP::OR(alu_type) => {
                let val = self.registers.a | self.read_alu(alu_type);
                self.set_flags(val == 0, false, false, false);
                self.write_r8(R8::A, val);
            }
            OP::XOR(alu_type) => {
                let val = self.registers.a ^ self.read_alu(alu_type);
                self.set_flags(val == 0, false, false, false);
                self.write_r8(R8::A, val);
            }
            OP::ADD(alu_type) => {
                let (val, h, c) = self
                    .read_r8(R8::A)
                    .overflowing_hc_add(self.read_alu(alu_type));
                self.set_flags(val == 0, false, h, c);
                self.write_r8(R8::A, val);
            }
            OP::ADC(alu_type) => {
                let (val, h1, c1) = self
                    .read_r8(R8::A)
                    .overflowing_hc_add(self.read_alu(alu_type));
                let (val, h2, c2) = val.overflowing_hc_add(self.registers.f.c as u8);
                self.set_flags(val == 0, false, h1 | h2, c1 | c2);
                self.write_r8(R8::A, val);
            }
            OP::ADDHL(word) => {
                let (val, h, c) = self
                    .read_word(Word::HL)
                    .overflowing_hc_add(self.read_word(word));
                self.set_flags(self.registers.f.z, false, h, c);
                self.write_word(Word::HL, val);
            }
            OP::ADDSPi8 => {
                let (val, h, c) = self.sp.overflowing_hc_add_i8(self.next_byte() as i8);
                self.set_flags(false, false, h, c);
                self.sp = val;
            }
            OP::SUB(alu_type) | OP::CP(alu_type) => {
                let (val, h, c) = self
                    .read_r8(R8::A)
                    .overflowing_hc_sub(self.read_alu(alu_type));
                self.set_flags(val == 0, true, h, c);
                if let OP::SUB(_) = instr.op {
                    self.write_r8(R8::A, val);
                }
            }
            OP::SBC(alu_type) => {
                let (val, h1, c1) = self
                    .read_r8(R8::A)
                    .overflowing_hc_sub(self.read_alu(alu_type));
                let (val, h2, c2) = val.overflowing_hc_sub(self.registers.f.c as u8);
                self.set_flags(val == 0, true, h1 | h2, c1 | c2);
                self.write_r8(R8::A, val);
            }
            OP::INC(inc_dec) => match inc_dec {
                IncDecTarget::R8(r8) => {
                    let (val, h, _) = self.read_r8(r8).overflowing_hc_add(1);
                    self.set_flags(val == 0, false, h, self.registers.f.c);
                    self.write_r8(r8, val);
                }
                IncDecTarget::Word(w) => self.write_word(w, self.read_word(w).wrapping_add(1)),
            },
            OP::DEC(inc_dec) => match inc_dec {
                IncDecTarget::R8(r8) => {
                    let (val, h, _) = self.read_r8(r8).overflowing_hc_sub(1);
                    self.set_flags(val == 0, true, h, self.registers.f.c);
                    self.write_r8(r8, val);
                }
                IncDecTarget::Word(w) => self.write_word(w, self.read_word(w).wrapping_sub(1)),
            },
            OP::CPL => {
                self.set_flags(self.registers.f.z, true, true, self.registers.f.c);
                self.write_r8(R8::A, self.read_r8(R8::A) ^ 0xFF);
            }
            OP::SCF => self.set_flags(self.registers.f.z, false, false, true),
            OP::CCF => self.set_flags(self.registers.f.z, false, false, !self.registers.f.c),

            OP::BIT(bit, r8) => {
                let res = self.read_r8(r8);
                let bit = bit as u8;
                self.set_flags((res & (1 << bit)) == 0, false, true, self.registers.f.c);
            }
            OP::SWAP(r8) => {
                let val = self.read_r8(r8).rotate_right(4);
                self.write_r8(r8, val);
                self.set_flags(val == 0, false, false, false);
            }
            OP::SET(bit, r8) => self.write_r8(r8, self.read_r8(r8) | (1 << (bit as u8))),
            OP::RES(bit, r8) => self.write_r8(r8, self.read_r8(r8) & !(1 << (bit as u8))),
            OP::RL(r8) => self.shift_left(r8, self.registers.f.c),
            OP::RLC(r8) => self.shift_left(r8, self.read_r8(r8) & (1 << 7) != 0),
            OP::SLA(r8) => self.shift_left(r8, false),
            OP::RR(r8) => self.shift_right(r8, self.registers.f.c),
            OP::RRC(r8) => self.shift_right(r8, self.read_r8(r8) & 1 != 0),
            OP::SRA(r8) => self.shift_right(r8, self.read_r8(r8) & (1 << 7) != 0),
            OP::SRL(r8) => self.shift_right(r8, false),
            OP::RLA => {
                self.shift_left(R8::A, self.registers.f.c);
                self.registers.f.z = false;
            }
            OP::RLCA => {
                self.shift_left(R8::A, self.read_r8(R8::A) & (1 << 7) != 0);
                self.registers.f.z = false;
            }
            OP::RRA => {
                self.shift_right(R8::A, self.registers.f.c);
                self.registers.f.z = false;
            }
            OP::RRCA => {
                self.shift_right(R8::A, self.read_r8(R8::A) & 1 != 0);
                self.registers.f.z = false;
            }
            OP::DAA => {
                let mut val = self.registers.a;
                let mut c = self.registers.f.c;
                // addition
                if !self.registers.f.n {
                    if self.registers.f.c || val > 0x99 {
                        c = true;
                        val = val.wrapping_add(0x60);
                    }
                    if self.registers.f.h || (val & 0xf) > 0x9 {
                        val = val.wrapping_add(0x06);
                    }
                }
                // subtraction
                else {
                    if self.registers.f.c {
                        val = val.wrapping_sub(0x60);
                    }
                    if self.registers.f.h {
                        val = val.wrapping_sub(0x06);
                    }
                };
                self.write_r8(R8::A, val);
                self.set_flags(val == 0, self.registers.f.n, false, c);
            }

            OP::JR(condition) => {
                if self.check_branch_condition(condition) {
                    next_pc = signed_offset_u16(next_pc, self.next_byte());
                    instr.cycles += 4;
                }
            }
            OP::JP(condition) => {
                if self.check_branch_condition(condition) {
                    next_pc = self.next_word();
                    instr.cycles += 12;
                }
            }
            OP::JPHL => next_pc = self.read_word(Word::HL),
            OP::CALL(condition) => {
                if self.check_branch_condition(condition) {
                    self.push_word(next_pc);
                    next_pc = self.next_word();
                    instr.cycles += 12;
                }
            }
            OP::RST(addr) => {
                self.push_word(next_pc);
                next_pc = addr;
            }
            OP::RET(condition) => {
                if self.check_branch_condition(condition) {
                    next_pc = self.pop_word();
                    instr.cycles += 12;
                }
            }
            OP::RETI => {
                self.ime = true;
                next_pc = self.pop_word();
                println!("RETI");
            }

            OP::PUSH(push_pop_target) => self.push_word(self.read_push_pop(push_pop_target)),
            OP::POP(push_pop_target) => {
                let val = self.pop_word();
                self.write_push_pop(push_pop_target, val);
            }

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

    fn shift_left(&mut self, r8: R8, bit: bool) {
        let val = self.read_r8(r8);
        let res = (val << 1) | (bit as u8);
        self.set_flags(res == 0, false, false, val & (1 << 7) != 0);
        self.write_r8(r8, res);
    }

    fn shift_right(&mut self, r8: R8, bit: bool) {
        let val = self.read_r8(r8);
        let res = (val >> 1) | ((bit as u8) << 7);
        self.set_flags(res == 0, false, false, val & 1 != 0);
        self.write_r8(r8, res);
    }

    fn check_branch_condition(&self, condition: BranchCondition) -> bool {
        match condition {
            BranchCondition::NotZero => !self.read_flag(Flag::Z),
            BranchCondition::Zero => self.read_flag(Flag::Z),
            BranchCondition::NotCarry => !self.read_flag(Flag::C),
            BranchCondition::Carry => self.read_flag(Flag::C),
            BranchCondition::Always => true,
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

    fn set_flags(&mut self, z: bool, n: bool, h: bool, c: bool) {
        self.registers.f.z = z;
        self.registers.f.n = n;
        self.registers.f.h = h;
        self.registers.f.c = c;
    }

    fn read_r8(&self, r8: R8) -> u8 {
        match r8 {
            R8::A => self.registers.a,
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HLInd => self.bus.read_byte(self.read_word(Word::HL)),
        }
    }

    fn write_r8(&mut self, r8: R8, val: u8) {
        match r8 {
            R8::A => self.registers.a = val,
            R8::B => self.registers.b = val,
            R8::C => self.registers.c = val,
            R8::D => self.registers.d = val,
            R8::E => self.registers.e = val,
            R8::H => self.registers.h = val,
            R8::L => self.registers.l = val,
            R8::HLInd => self.bus.write_byte(self.read_word(Word::HL), val),
        }
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

    fn read_push_pop(&self, p: PushPopTarget) -> u16 {
        match p {
            PushPopTarget::BC => self.read_word(Word::BC),
            PushPopTarget::DE => self.read_word(Word::DE),
            PushPopTarget::HL => self.read_word(Word::HL),
            PushPopTarget::AF => {
                ((self.registers.a as u16) << 8) | u8::from(self.registers.f) as u16
            }
        }
    }

    fn write_push_pop(&mut self, p: PushPopTarget, val: u16) {
        match p {
            PushPopTarget::BC => self.write_word(Word::BC, val),
            PushPopTarget::DE => self.write_word(Word::DE, val),
            PushPopTarget::HL => self.write_word(Word::HL, val),
            PushPopTarget::AF => {
                self.registers.a = (val >> 8) as u8;
                self.registers.f = Flags::from(val as u8);
            }
        }
    }

    fn read_alu(&self, alu_type: ALUType) -> u8 {
        match alu_type {
            ALUType::R8(r8) => self.read_r8(r8),
            ALUType::Imm8 => self.next_byte(),
        }
    }

    fn read_ind(&mut self, ind: Indirect) -> u8 {
        match ind {
            Indirect::CInd => self.bus.read_byte(0xFF00 + self.read_r8(R8::C) as u16),
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
                .write_byte(0xFF00 + self.read_r8(R8::C) as u16, val),
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
    bootrom: [u8; 0x100],
    gpu: GPU,
}

impl MemoryBus {
    fn read_byte(&self, addr: u16) -> u8 {
        if addr < 0x100 && self.memory[0xff50] == 0 {
            return self.bootrom[addr as usize];
        }
        self.memory[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, val: u8) {
        if addr >= 0x8000 {
            self.memory[addr as usize] = val
        }
    }

    fn read_bytes(&self, addr: u16, len: u16) -> Vec<u8> {
        let mut slice: Vec<u8> = vec![0; len as usize];
        for i in 0..len {
            slice[i as usize] = self.read_byte(addr + i);
        }
        slice
    }

    fn read_word(&self, addr: u16) -> u16 {
        ((self.read_byte(addr + 1) as u16) << 8) | (self.read_byte(addr) as u16)
    }

    fn write_word(&mut self, addr: u16, val: u16) {
        self.write_byte(addr, val as u8);
        self.write_byte(addr + 1, (val >> 8) as u8)
    }
}
