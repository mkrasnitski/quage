use crate::utils::*;
use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Default, Serialize, Deserialize)]
pub struct Flags {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}

impl From<Flags> for u8 {
    fn from(f: Flags) -> u8 {
        ((f.z as u8) << 7) | ((f.n as u8) << 6) | ((f.h as u8) << 5) | ((f.c as u8) << 4)
    }
}

impl From<u8> for Flags {
    fn from(f: u8) -> Self {
        Flags {
            z: f.bit(7),
            n: f.bit(6),
            h: f.bit(5),
            c: f.bit(4),
        }
    }
}

impl std::fmt::Display for Flags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let z = if self.z { "Z" } else { "-" };
        let n = if self.n { "N" } else { "-" };
        let h = if self.h { "H" } else { "-" };
        let c = if self.c { "C" } else { "-" };
        write!(f, "{z}{n}{h}{c}")
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

pub trait HalfCarryI8 {
    fn overflowing_hc_add_i8(&self, rhs: i8) -> (Self, bool, bool)
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
        let h = ((self & 0xfff) + (rhs & 0xfff)) & 0x1000 == 0x1000;
        (v, h, c)
    }

    fn overflowing_hc_sub(&self, rhs: u16) -> (u16, bool, bool) {
        let (v, c) = self.overflowing_sub(rhs);
        let h = (self & 0xfff) < (rhs & 0xfff);
        (v, h, c)
    }
}

impl HalfCarryI8 for u16 {
    fn overflowing_hc_add_i8(&self, rhs: i8) -> (u16, bool, bool) {
        let (lo, h, c) = (*self as u8).overflowing_hc_add(rhs as u8);
        let (hi, _, _) = if rhs >= 0 {
            ((self >> 8) as u8).overflowing_hc_add(c as u8)
        } else {
            ((self >> 8) as u8).overflowing_hc_sub(!c as u8)
        };
        let val = ((hi as u16) << 8) | lo as u16;
        (val, h, c)
    }
}
