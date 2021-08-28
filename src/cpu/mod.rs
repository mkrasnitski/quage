#![allow(non_snake_case)]
mod debug;
mod flags;
mod instruction;

use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::bus::MemoryBus;
use crate::config::Config;
use crate::utils::*;

use flags::*;
use instruction::*;

#[derive(Default, Serialize, Deserialize)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: Flags,
    h: u8,
    l: u8,

    queue_ime: bool,
    ime: bool,
    halted: bool,
    halt_bug: bool,
}

impl std::fmt::Debug for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} [{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}]",
            self.f, self.a, self.b, self.c, self.d, self.e, self.h, self.l
        )
    }
}

#[derive(Serialize, Deserialize)]
pub struct CPU {
    pub bus: MemoryBus,
    registers: Registers,
    pc: u16,
    sp: u16,
    cycles: u64,
    debug: bool,
}

impl std::fmt::Debug for CPU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{: >2} {:04x} {:04x} {:?}",
            self.cycles, self.pc, self.sp, self.registers
        )
    }
}

impl CPU {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>, config: &Config) -> Result<Self> {
        let mut cpu = CPU {
            bus: MemoryBus::new(bootrom, cartridge)?,
            registers: Registers::default(),
            pc: 0,
            sp: 0,
            cycles: 0,
            debug: config.debug,
        };
        if config.skip_bootrom {
            cpu.registers.a = 0x01;
            cpu.registers.c = 0x13;
            cpu.registers.e = 0xd8;
            cpu.registers.h = 0x01;
            cpu.registers.l = 0x4d;
            cpu.registers.f = Flags::from(0xb0);
            cpu.sp = 0xfffe;
            cpu.bus.timers.DIV = 0xABE4; // one of ABCC-ABCF is the actual right value here
            cpu.bus.write_byte(0xff10, 0x80);
            cpu.bus.write_byte(0xff11, 0xbf);
            cpu.bus.write_byte(0xff12, 0xf3);
            cpu.bus.write_byte(0xff14, 0xbf);
            cpu.bus.write_byte(0xff16, 0x3f);
            cpu.bus.write_byte(0xff19, 0xbf);
            cpu.bus.write_byte(0xff1a, 0x7f);
            cpu.bus.write_byte(0xff1b, 0xff);
            cpu.bus.write_byte(0xff1c, 0x9f);
            cpu.bus.write_byte(0xff1e, 0xbf);
            cpu.bus.write_byte(0xff20, 0xff);
            cpu.bus.write_byte(0xff23, 0xbf);
            cpu.bus.write_byte(0xff24, 0x77);
            cpu.bus.write_byte(0xff25, 0xf3);
            cpu.bus.write_byte(0xff26, 0xf1);
            cpu.bus.write_byte(0xff40, 0x91);
            cpu.bus.write_byte(0xff47, 0xfc);
            cpu.bus.write_byte(0xff48, 0xff);
            cpu.bus.write_byte(0xff49, 0xff);
            cpu.bus.write_byte(0xff50, 0x01);
            cpu.pc = 0x100;
        }
        Ok(cpu)
    }

    pub fn reset(&mut self, config: &Config) -> Result<()> {
        let mut cpu = CPU::new(
            self.bus.bootrom.clone(),
            self.bus.cartridge.contents.clone(),
            config,
        )?;
        cpu.bus.cartridge.ram = self.bus.cartridge.ram.clone();
        cpu.bus.cartridge.mapper.rtc = self.bus.cartridge.mapper.rtc.clone();
        *self = cpu;
        Ok(())
    }

    pub fn step(&mut self) -> Result<()> {
        self.check_interrupts();
        if self.registers.halted {
            self.tick_mclock();
        } else {
            let state = self.debug.then(|| format!("{:?}", self));
            let start_cycle = self.cycles;
            let mut instr = self.parse_next_instruction()?;
            self.execute(&mut instr);
            assert!(self.cycles == start_cycle + instr.cycles, "{}", instr);
            if let Some(s) = state {
                println!("{} {}", s, instr);
            }
        }
        Ok(())
    }

    fn parse_next_instruction(&mut self) -> Result<Instruction> {
        let mut byte = self.consume_byte();
        let mut prefix = false;
        let (op, (len, cycles)) = if byte == 0xcb {
            byte = self.consume_byte();
            prefix = true;
            OP::from_prefix_byte(byte)
        } else {
            OP::from_byte(byte)?
        };
        let mut bytes = Vec::with_capacity(len as usize);
        if prefix {
            bytes.push(0xcb);
        }
        bytes.push(byte);
        for _ in bytes.len()..len as usize {
            bytes.push(self.consume_byte());
        }
        Ok(Instruction {
            op,
            len,
            cycles,
            bytes,
        })
    }

    fn consume_byte(&mut self) -> u8 {
        let byte = self.read_addr(self.pc);
        if self.registers.halt_bug {
            self.registers.halt_bug = false;
        } else {
            self.pc += 1;
        }
        byte
    }

    fn tick_mclock(&mut self) {
        self.increment_timers();
        self.bus.increment_dma();
        self.bus.cartridge.increment_rtc();
        self.bus.ppu.draw();
        self.cycles += 4;
    }

    fn increment_timers(&mut self) {
        if self.bus.timers.increment() {
            self.request_interrupt(2);
        }
    }

    fn request_interrupt(&mut self, int: u8) {
        if int < 5 {
            self.bus
                .write_byte(0xFF0F, self.bus.read_byte(0xFF0F) | (1 << int));
        }
    }

    fn check_interrupts(&mut self) {
        self.bus.check_interrupts();
        let mut intr_cancelled = false;
        for i in 0..5 {
            if (self.bus.IE & self.bus.IF).bit(i) {
                self.registers.halted = false;
                if self.registers.ime {
                    // An ISR takes 5 m-cycles to service, like on Z80
                    // 2 NOPs, then 2 cycles pushing PC onto the stack,
                    // then 1 more cycle to change PC

                    // NOTE: THESE TWO EXTRA NOPS CAUSE REGRESSIONS ALL OVER THE PLACE
                    self.tick_mclock();
                    self.tick_mclock();
                    // ISR is cancelled for this specific interrupt if IE is written to
                    // during the high byte push and the corresponding bit is no longer set.
                    self.push((self.pc >> 8) as u8);
                    if (self.bus.IE & self.bus.IF).bit(i) {
                        self.push(self.pc as u8);
                        self.registers.ime = false;
                        self.bus.IF &= !(1 << i);
                        self.pc = (i << 3) as u16 + 0x40;
                        self.tick_mclock();
                        intr_cancelled = false;
                        break;
                    } else {
                        intr_cancelled = true;
                    }
                }
            }
        }
        if intr_cancelled {
            self.registers.ime = false;
            self.pc = 0x0000;
            self.tick_mclock();
        }
        if self.registers.queue_ime {
            self.registers.ime = true;
            self.registers.queue_ime = false;
        }
    }

    fn execute(&mut self, instr: &mut Instruction) {
        match instr.op {
            OP::NOP => (),
            OP::STOP => panic!("STOP"),
            OP::HALT => {
                let IE = self.bus.read_byte(0xFFFF);
                let IF = self.bus.read_byte(0xFF0F);
                if !self.registers.ime && (IE & IF & 0x1F) != 0 {
                    self.registers.halt_bug = true;
                } else {
                    self.registers.halted = true;
                }
            }
            OP::DI => self.registers.ime = false,
            OP::EI => self.registers.queue_ime = true,

            OP::LD(ld_type) => match ld_type {
                LDType::ByteImm(r8) => self.write_r8(r8, instr.byte_arg()),
                LDType::WordImm(word) => self.write_word(word, instr.word_arg()),
                LDType::IndFromA(ind) => {
                    let ind = self.decode_ind(ind);
                    let A = self.read_r8(R8::A);
                    self.write_addr(ind, A);
                }
                LDType::AFromInd(ind) => {
                    let ind = self.decode_ind(ind);
                    let val = self.read_addr(ind);
                    self.write_r8(R8::A, val);
                }
                LDType::MoveByte(dest, src) => {
                    let A = self.read_r8(src);
                    self.write_r8(dest, A);
                }
                LDType::ByteIndFromA => {
                    let A = self.read_r8(R8::A);
                    self.write_addr(0xFF00 + instr.byte_arg() as u16, A);
                }
                LDType::AFromByteInd => {
                    let val = self.read_addr(0xFF00 + instr.byte_arg() as u16);
                    self.write_r8(R8::A, val);
                }
                LDType::AddrFromA => {
                    let A = self.read_r8(R8::A);
                    self.write_addr(instr.word_arg(), A);
                }
                LDType::AFromAddr => {
                    let val = self.read_addr(instr.word_arg());
                    self.write_r8(R8::A, val);
                }
                LDType::SPFromHL => {
                    self.write_word(Word::SP, self.read_word(Word::HL));
                    self.tick_mclock();
                }
                LDType::AddrFromSP => {
                    let arg = instr.word_arg();
                    let sp = self.read_word(Word::SP);
                    self.write_addr(arg, sp as u8);
                    self.write_addr(arg + 1, (sp >> 8) as u8);
                }
                LDType::HLFromSPi8 => {
                    let (val, h, c) = self.sp.overflowing_hc_add_i8(instr.byte_arg() as i8);
                    self.set_flags(false, false, h, c);
                    self.write_word(Word::HL, val);
                    self.tick_mclock();
                }
            },

            OP::AND(alu_type) => {
                let val = self.registers.a & self.read_alu(alu_type, instr);
                self.set_flags(val == 0, false, true, false);
                self.write_r8(R8::A, val);
            }
            OP::OR(alu_type) => {
                let val = self.registers.a | self.read_alu(alu_type, instr);
                self.set_flags(val == 0, false, false, false);
                self.write_r8(R8::A, val);
            }
            OP::XOR(alu_type) => {
                let val = self.registers.a ^ self.read_alu(alu_type, instr);
                self.set_flags(val == 0, false, false, false);
                self.write_r8(R8::A, val);
            }
            OP::ADD(alu_type) => {
                let (val, h, c) = self
                    .read_r8(R8::A)
                    .overflowing_hc_add(self.read_alu(alu_type, instr));
                self.set_flags(val == 0, false, h, c);
                self.write_r8(R8::A, val);
            }
            OP::ADC(alu_type) => {
                let (val, h1, c1) = self
                    .read_r8(R8::A)
                    .overflowing_hc_add(self.read_alu(alu_type, instr));
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
                self.tick_mclock();
            }
            OP::ADDSPi8 => {
                let (val, h, c) = self.sp.overflowing_hc_add_i8(instr.byte_arg() as i8);
                self.set_flags(false, false, h, c);
                self.sp = val;
                self.tick_mclock();
                self.tick_mclock();
            }
            OP::SUB(alu_type) | OP::CP(alu_type) => {
                let (val, h, c) = self
                    .read_r8(R8::A)
                    .overflowing_hc_sub(self.read_alu(alu_type, instr));
                self.set_flags(val == 0, true, h, c);
                if let OP::SUB(_) = instr.op {
                    self.write_r8(R8::A, val);
                }
            }
            OP::SBC(alu_type) => {
                let (val, h1, c1) = self
                    .read_r8(R8::A)
                    .overflowing_hc_sub(self.read_alu(alu_type, instr));
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
                IncDecTarget::Word(w) => {
                    self.write_word(w, self.read_word(w).wrapping_add(1));
                    self.tick_mclock();
                }
            },
            OP::DEC(inc_dec) => match inc_dec {
                IncDecTarget::R8(r8) => {
                    let (val, h, _) = self.read_r8(r8).overflowing_hc_sub(1);
                    self.set_flags(val == 0, true, h, self.registers.f.c);
                    self.write_r8(r8, val);
                }
                IncDecTarget::Word(w) => {
                    self.write_word(w, self.read_word(w).wrapping_sub(1));
                    self.tick_mclock();
                }
            },
            OP::CPL => {
                self.set_flags(self.registers.f.z, true, true, self.registers.f.c);
                let A = self.read_r8(R8::A);
                self.write_r8(R8::A, A ^ 0xFF);
            }
            OP::SCF => self.set_flags(self.registers.f.z, false, false, true),
            OP::CCF => self.set_flags(self.registers.f.z, false, false, !self.registers.f.c),

            OP::BIT(bit, r8) => {
                let res = self.read_r8(r8);
                self.set_flags(!res.bit(bit as u8), false, true, self.registers.f.c);
            }
            OP::SWAP(r8) => {
                let val = self.read_r8(r8).rotate_right(4);
                self.write_r8(r8, val);
                self.set_flags(val == 0, false, false, false);
            }
            OP::SET(bit, r8) => {
                let val = self.read_r8(r8);
                self.write_r8(r8, val | (1 << (bit as u8)));
            }
            OP::RES(bit, r8) => {
                let val = self.read_r8(r8);
                self.write_r8(r8, val & !(1 << (bit as u8)));
            }
            OP::RL(r8) => {
                let c = self.registers.f.c;
                self.shift_left(r8, |_| c);
            }
            OP::RLC(r8) => self.shift_left(r8, |val| val.bit(7)),
            OP::SLA(r8) => self.shift_left(r8, |_| false),
            OP::RR(r8) => {
                let c = self.registers.f.c;
                self.shift_right(r8, |_| c);
            }
            OP::RRC(r8) => self.shift_right(r8, |val| val.bit(0)),
            OP::SRA(r8) => self.shift_right(r8, |val| val.bit(7)),
            OP::SRL(r8) => self.shift_right(r8, |_| false),
            OP::RLA => {
                let c = self.registers.f.c;
                self.shift_left(R8::A, |_| c);
                self.registers.f.z = false;
            }
            OP::RLCA => {
                self.shift_left(R8::A, |val| val.bit(7));
                self.registers.f.z = false;
            }
            OP::RRA => {
                let c = self.registers.f.c;
                self.shift_right(R8::A, |_| c);
                self.registers.f.z = false;
            }
            OP::RRCA => {
                self.shift_right(R8::A, |val| val.bit(0));
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
                let arg = instr.byte_arg() as i8 as u16;
                if self.check_branch_condition(condition) {
                    self.tick_mclock();
                    self.pc = self.pc.wrapping_add(arg);
                    instr.cycles += 4;
                }
            }
            OP::JP(condition) => {
                let arg = instr.word_arg();
                if self.check_branch_condition(condition) {
                    self.tick_mclock();
                    self.pc = arg;
                    instr.cycles += 4;
                }
            }
            OP::JPHL => self.pc = self.read_word(Word::HL),
            OP::CALL(condition) => {
                let arg = instr.word_arg();
                if self.check_branch_condition(condition) {
                    self.tick_mclock();
                    self.push_word(self.pc);
                    self.pc = arg;
                    instr.cycles += 12;
                }
            }
            OP::RST(addr) => {
                self.push_word(self.pc);
                self.pc = addr;
                self.tick_mclock();
            }
            OP::RET(condition) => {
                match condition {
                    BranchCondition::Always => {}
                    _ => self.tick_mclock(),
                };
                if self.check_branch_condition(condition) {
                    self.tick_mclock();
                    self.pc = self.pop_word();
                    instr.cycles += 12;
                }
            }
            OP::RETI => {
                self.registers.ime = true;
                self.pc = self.pop_word();
                self.tick_mclock();
            }

            OP::PUSH(push_pop_target) => {
                let val = match push_pop_target {
                    PushPopTarget::BC => self.read_word(Word::BC),
                    PushPopTarget::DE => self.read_word(Word::DE),
                    PushPopTarget::HL => self.read_word(Word::HL),
                    PushPopTarget::AF => {
                        ((self.registers.a as u16) << 8) | u8::from(self.registers.f) as u16
                    }
                };
                self.tick_mclock();
                self.push_word(val);
            }
            OP::POP(push_pop_target) => {
                let val = self.pop_word();
                match push_pop_target {
                    PushPopTarget::BC => self.write_word(Word::BC, val),
                    PushPopTarget::DE => self.write_word(Word::DE, val),
                    PushPopTarget::HL => self.write_word(Word::HL, val),
                    PushPopTarget::AF => {
                        self.registers.a = (val >> 8) as u8;
                        self.registers.f = Flags::from(val as u8);
                    }
                }
            }
        };
    }

    fn pop(&mut self) -> u8 {
        let res = self.bus.read_byte(self.sp);
        self.sp += 1;
        self.tick_mclock();
        res
    }

    fn push(&mut self, val: u8) {
        self.sp -= 1;
        self.tick_mclock();
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

    fn shift_left(&mut self, r8: R8, carry: impl Fn(u8) -> bool) {
        let val = self.read_r8(r8);
        let res = (val << 1) | (carry(val) as u8);
        self.set_flags(res == 0, false, false, val.bit(7));
        self.write_r8(r8, res);
    }

    fn shift_right(&mut self, r8: R8, carry: impl Fn(u8) -> bool) {
        let val = self.read_r8(r8);
        let res = (val >> 1) | ((carry(val) as u8) << 7);
        self.set_flags(res == 0, false, false, val.bit(0));
        self.write_r8(r8, res);
    }

    fn check_branch_condition(&self, condition: BranchCondition) -> bool {
        match condition {
            BranchCondition::NotZero => !self.registers.f.z,
            BranchCondition::Zero => self.registers.f.z,
            BranchCondition::NotCarry => !self.registers.f.c,
            BranchCondition::Carry => self.registers.f.c,
            BranchCondition::Always => true,
        }
    }

    fn set_flags(&mut self, z: bool, n: bool, h: bool, c: bool) {
        self.registers.f.z = z;
        self.registers.f.n = n;
        self.registers.f.h = h;
        self.registers.f.c = c;
    }

    fn read_r8(&mut self, r8: R8) -> u8 {
        match r8 {
            R8::A => self.registers.a,
            R8::B => self.registers.b,
            R8::C => self.registers.c,
            R8::D => self.registers.d,
            R8::E => self.registers.e,
            R8::H => self.registers.h,
            R8::L => self.registers.l,
            R8::HLInd => self.read_addr(self.read_word(Word::HL)),
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
            R8::HLInd => self.write_addr(self.read_word(Word::HL), val),
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

    fn read_addr(&mut self, addr: u16) -> u8 {
        let val = self.bus.read_byte(addr);
        self.tick_mclock();
        val
    }

    fn write_addr(&mut self, addr: u16, val: u8) {
        self.bus.write_byte(addr, val);
        self.tick_mclock();
    }

    fn read_alu(&mut self, alu_type: ALUType, instr: &mut Instruction) -> u8 {
        match alu_type {
            ALUType::R8(r8) => self.read_r8(r8),
            ALUType::Imm8 => instr.byte_arg(),
        }
    }

    fn decode_ind(&mut self, ind: Indirect) -> u16 {
        match ind {
            Indirect::CInd => 0xFF00 + self.read_r8(R8::C) as u16,
            Indirect::BCInd => self.read_word(Word::BC),
            Indirect::DEInd => self.read_word(Word::DE),
            Indirect::HLIndPlus => {
                let hl = self.read_word(Word::HL);
                self.write_word(Word::HL, hl + 1);
                hl
            }
            Indirect::HLIndMinus => {
                let hl = self.read_word(Word::HL);
                self.write_word(Word::HL, hl - 1);
                hl
            }
        }
    }
}
