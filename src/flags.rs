#![allow(dead_code)]

#[derive(Copy, Clone)]
pub struct Flags {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}

pub enum Flag {
    Z,
    N,
    H,
    C,
}

impl std::convert::From<Flags> for u8 {
    fn from(f: Flags) -> u8 {
        ((f.z as u8) << 7) | ((f.n as u8) << 6) | ((f.h as u8) << 5) | ((f.c as u8) << 4)
    }
}

impl std::convert::From<u8> for Flags {
    fn from(f: u8) -> Flags {
        Flags {
            z: (f & (1 << 7)) != 0,
            n: (f & (1 << 6)) != 0,
            h: (f & (1 << 5)) != 0,
            c: (f & (1 << 4)) != 0,
        }
    }
}

pub trait HalfCarry {
    fn overflowing_hc_add(&self, rhs: Self) -> (Self, bool, bool)
    where
        Self: Sized;

    fn overflowing_hc_sub(&self, rhs: Self) -> (Self, bool, bool)
    where
        Self: Sized;
}

impl HalfCarry for u8 {
    fn overflowing_hc_add(&self, rhs: u8) -> (u8, bool, bool) {
        let (v, c) = self.overflowing_add(rhs);
        let h = ((self & 0xf) + (rhs & 0xf)) & 0x10 == 0x10;
        (v, h, c)
    }

    fn overflowing_hc_sub(&self, rhs: u8) -> (u8, bool, bool) {
        let (v, c) = self.overflowing_sub(rhs);
        let h = (self & 0xf) < (rhs & 0xf);
        (v, h, c)
    }
}

impl HalfCarry for u16 {
    fn overflowing_hc_add(&self, rhs: u16) -> (u16, bool, bool) {
        let (v, c) = self.overflowing_add(rhs);
        let h = ((self & 0xff) + (rhs & 0xff)) & 0x100 == 0x100;
        (v, h, c)
    }

    fn overflowing_hc_sub(&self, rhs: u16) -> (u16, bool, bool) {
        let (v, c) = self.overflowing_sub(rhs);
        let h = (self & 0xff) < (rhs & 0xff);
        (v, h, c)
    }
}
