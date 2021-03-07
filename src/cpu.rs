#![allow(non_snake_case)]
use anyhow::Result;

use crate::bus::*;
use crate::display::*;
use crate::flags::*;
use crate::instruction::*;

const DEBUG: bool = false;

#[derive(Default)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: Flags,
    h: u8,
    l: u8,
}

pub struct CPU {
    pc: u16,
    sp: u16,
    cycles: u64,
    queue_ime: bool,
    ime: bool,
    halted: bool,
    halt_bug: bool,
    registers: Registers,
    bus: MemoryBus,
}

impl CPU {
    pub fn new(bootrom: Vec<u8>, cartridge: Vec<u8>, skip_bootrom: bool) -> Result<Self> {
        let mut cpu = CPU {
            pc: 0,
            sp: 0,
            cycles: 0,
            queue_ime: false,
            ime: false,
            halted: false,
            halt_bug: false,
            registers: Registers::default(),
            bus: MemoryBus::new(bootrom, cartridge)?,
        };
        if skip_bootrom {
            cpu.registers.a = 0x01;
            cpu.registers.c = 0x13;
            cpu.registers.e = 0xd8;
            cpu.registers.h = 0x01;
            cpu.registers.l = 0x4d;
            cpu.registers.f = Flags::from(0xb0);
            cpu.sp = 0xfffe;
            // TODO: figure out Timer reg values if skipping bootrom
            // cpu.bus.write_byte(0xff04, 0xAC);
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

    fn state(&self) -> String {
        let r = &self.registers;
        format!(
            "{: >2} {:04x} {:04x} {} [{:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}]",
            self.cycles, self.pc, self.sp, r.f, r.a, r.b, r.c, r.d, r.e, r.h, r.l,
        )
    }

    pub fn step(&mut self) -> Result<()> {
        self.check_interrupts();
        let cycles_passed = if self.halted {
            4
        } else {
            let state = self.state();
            let mut instr = self.parse_next_instruction()?;
            self.execute(&mut instr);
            if DEBUG {
                println!("{} {}", state, instr);
            }
            instr.cycles
        };
        for _ in 0..cycles_passed / 4 {
            self.increment_timers();
            self.bus.ppu.draw();
            self.bus.increment_rtc();
        }
        self.cycles += cycles_passed;
        Ok(())
    }

    fn parse_next_instruction(&mut self) -> Result<Instruction> {
        let byte = self.consume_byte();
        let (byte, prefix) = if byte == 0xcb {
            (self.consume_byte(), true)
        } else {
            (byte, false)
        };
        let mut instr = Instruction::from_byte(byte, prefix)?;
        let args = self.consume_bytes(instr.len - instr.bytes.len() as u8);
        instr.bytes.extend(args);
        Ok(instr)
    }

    pub fn poll_display_event(&mut self) -> DisplayEvent {
        self.bus.poll_display_event()
    }

    fn request_interrupt(&mut self, int: u8) {
        if int < 5 {
            self.bus
                .write_byte(0xFF0F, self.bus.read_byte(0xFF0F) | (1 << int));
        }
    }

    fn check_interrupts(&mut self) {
        self.bus.check_interrupts();
        let IE = self.bus.read_byte(0xFFFF);
        let IF = self.bus.read_byte(0xFF0F);
        for i in 0..5 {
            let mask = 1u8 << i;
            if IE & IF & mask != 0 {
                self.halted = false;
                if self.ime {
                    self.ime = false;
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

    fn increment_timers(&mut self) {
        for _ in 0..4 {
            if self.bus.timers.increment() {
                self.request_interrupt(2);
            }
        }
    }

    fn execute(&mut self, instr: &mut Instruction) {
        match instr.op {
            OP::NOP => (),
            OP::STOP => panic!("\nSTOP\n"),
            OP::HALT => {
                let IE = self.bus.read_byte(0xFFFF);
                let IF = self.bus.read_byte(0xFF0F);
                if !self.ime && (IE & IF & 0x1F) != 0 {
                    self.halt_bug = true;
                } else {
                    self.halted = true;
                }
            }
            OP::DI => self.ime = false,
            OP::EI => self.queue_ime = true,

            OP::LD(ld_type) => match ld_type {
                LDType::ByteImm(r8) => self.write_r8(r8, instr.byte_arg()),
                LDType::WordImm(word) => self.write_word(word, instr.word_arg()),
                LDType::IndFromA(ind) => {
                    let ind = self.decode_ind(ind);
                    self.bus.write_byte(ind, self.read_r8(R8::A));
                }
                LDType::AFromInd(ind) => {
                    let ind = self.decode_ind(ind);
                    self.write_r8(R8::A, self.bus.read_byte(ind));
                }
                LDType::MoveByte(dest, src) => self.write_r8(dest, self.read_r8(src)),
                LDType::ByteIndFromA => self
                    .bus
                    .write_byte(0xFF00 + instr.byte_arg() as u16, self.read_r8(R8::A)),
                LDType::AFromByteInd => {
                    self.write_r8(R8::A, self.bus.read_byte(0xFF00 + instr.byte_arg() as u16))
                }
                LDType::AddrFromA => self.bus.write_byte(instr.word_arg(), self.read_r8(R8::A)),
                LDType::AFromAddr => self.write_r8(R8::A, self.bus.read_byte(instr.word_arg())),
                LDType::SPFromHL => self.write_word(Word::SP, self.read_word(Word::HL)),
                LDType::AddrFromSP => self
                    .bus
                    .write_word(instr.word_arg(), self.read_word(Word::SP)),
                LDType::HLFromSPi8 => {
                    let (val, h, c) = self.sp.overflowing_hc_add_i8(instr.byte_arg() as i8);
                    self.set_flags(false, false, h, c);
                    self.write_word(Word::HL, val);
                }
            },

            OP::AND(alu_type) => {
                let val = self.registers.a & self.read_alu(alu_type, &instr);
                self.set_flags(val == 0, false, true, false);
                self.write_r8(R8::A, val);
            }
            OP::OR(alu_type) => {
                let val = self.registers.a | self.read_alu(alu_type, &instr);
                self.set_flags(val == 0, false, false, false);
                self.write_r8(R8::A, val);
            }
            OP::XOR(alu_type) => {
                let val = self.registers.a ^ self.read_alu(alu_type, &instr);
                self.set_flags(val == 0, false, false, false);
                self.write_r8(R8::A, val);
            }
            OP::ADD(alu_type) => {
                let (val, h, c) = self
                    .read_r8(R8::A)
                    .overflowing_hc_add(self.read_alu(alu_type, &instr));
                self.set_flags(val == 0, false, h, c);
                self.write_r8(R8::A, val);
            }
            OP::ADC(alu_type) => {
                let (val, h1, c1) = self
                    .read_r8(R8::A)
                    .overflowing_hc_add(self.read_alu(alu_type, &instr));
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
                let (val, h, c) = self.sp.overflowing_hc_add_i8(instr.byte_arg() as i8);
                self.set_flags(false, false, h, c);
                self.sp = val;
            }
            OP::SUB(alu_type) | OP::CP(alu_type) => {
                let (val, h, c) = self
                    .read_r8(R8::A)
                    .overflowing_hc_sub(self.read_alu(alu_type, &instr));
                self.set_flags(val == 0, true, h, c);
                if let OP::SUB(_) = instr.op {
                    self.write_r8(R8::A, val);
                }
            }
            OP::SBC(alu_type) => {
                let (val, h1, c1) = self
                    .read_r8(R8::A)
                    .overflowing_hc_sub(self.read_alu(alu_type, &instr));
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
                    self.pc = self.pc.wrapping_add(instr.byte_arg() as i8 as u16);
                    instr.cycles += 4;
                }
            }
            OP::JP(condition) => {
                if self.check_branch_condition(condition) {
                    self.pc = instr.word_arg();
                    instr.cycles += 4;
                }
            }
            OP::JPHL => self.pc = self.read_word(Word::HL),
            OP::CALL(condition) => {
                if self.check_branch_condition(condition) {
                    self.push_word(self.pc);
                    self.pc = instr.word_arg();
                    instr.cycles += 12;
                }
            }
            OP::RST(addr) => {
                self.push_word(self.pc);
                self.pc = addr;
            }
            OP::RET(condition) => {
                if self.check_branch_condition(condition) {
                    self.pc = self.pop_word();
                    instr.cycles += 12;
                }
            }
            OP::RETI => {
                self.ime = true;
                self.pc = self.pop_word();
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

    fn consume_byte(&mut self) -> u8 {
        let byte = self.bus.read_byte(self.pc);
        if self.halt_bug {
            self.halt_bug = false;
        } else {
            self.pc += 1;
        }
        byte
    }

    fn consume_bytes(&mut self, num_bytes: u8) -> Vec<u8> {
        (0..num_bytes).map(|_| self.consume_byte()).collect()
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

    fn read_alu(&mut self, alu_type: ALUType, instr: &Instruction) -> u8 {
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
