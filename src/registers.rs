use std::fmt::{self, Display, Formatter};

#[derive(Copy, Clone)]
pub enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl Display for Reg8 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Reg8::A => "A",
                Reg8::B => "B",
                Reg8::C => "C",
                Reg8::D => "D",
                Reg8::E => "E",
                Reg8::H => "H",
                Reg8::L => "L",
            }
        )
    }
}

#[derive(Copy, Clone)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

impl Display for Reg16 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Reg16::AF => "AF",
                Reg16::BC => "BC",
                Reg16::DE => "DE",
                Reg16::HL => "HL",
                Reg16::SP => "SP",
                Reg16::PC => "PC",
            }
        )
    }
}

#[derive(Copy, Clone)]
pub enum Flag {
    Z,
    N,
    H,
    C,
}

impl Display for Flag {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Flag::Z => "Z",
                Flag::N => "N",
                Flag::H => "H",
                Flag::C => "C",
            }
        )
    }
}

pub struct Registers {
    a: u8,
    f: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            a: 0,
            f: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            sp: 0,
            pc: 0,
        }
    }

    pub fn get_8bit(&self, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.a,
            Reg8::B => self.b,
            Reg8::C => self.c,
            Reg8::D => self.d,
            Reg8::E => self.e,
            Reg8::H => self.h,
            Reg8::L => self.l,
        }
    }

    pub fn set_8bit(&mut self, reg: Reg8, val: u8) {
        match reg {
            Reg8::A => self.a = val,
            Reg8::B => self.b = val,
            Reg8::C => self.c = val,
            Reg8::D => self.d = val,
            Reg8::E => self.e = val,
            Reg8::H => self.h = val,
            Reg8::L => self.l = val,
        }
    }

    pub fn get_16bit(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::AF => ((self.a as u16) << 8) | (self.f as u16),
            Reg16::BC => ((self.b as u16) << 8) | (self.c as u16),
            Reg16::DE => ((self.d as u16) << 8) | (self.e as u16),
            Reg16::HL => ((self.h as u16) << 8) | (self.l as u16),
            Reg16::SP => self.sp,
            Reg16::PC => self.pc,
        }
    }

    pub fn set_16bit(&mut self, reg: Reg16, val: u16) {
        match reg {
            Reg16::AF => {
                self.a = (val >> 8) as u8;
                self.f = val as u8;
            }
            Reg16::BC => {
                self.b = (val >> 8) as u8;
                self.c = val as u8;
            }
            Reg16::DE => {
                self.d = (val >> 8) as u8;
                self.e = val as u8;
            }
            Reg16::HL => {
                self.h = (val >> 8) as u8;
                self.l = val as u8;
            }
            Reg16::SP => self.sp = val,
            Reg16::PC => self.pc = val,
        }
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        let bitmask = match flag {
            Flag::Z => 0b10000000,
            Flag::N => 0b01000000,
            Flag::H => 0b00100000,
            Flag::C => 0b00010000,
        };

        self.f & bitmask == bitmask
    }

    pub fn set_flag(&mut self, flag: Flag, set: bool) {
        let bitmask = match flag {
            Flag::Z => 0b10000000,
            Flag::N => 0b01000000,
            Flag::H => 0b00100000,
            Flag::C => 0b00010000,
        };

        if set {
            self.f |= bitmask;
        } else {
            self.f &= !bitmask;
        }
    }

    pub fn get_pc_offset(&self, offset: i8) -> u16 {
        ((self.pc as i16).wrapping_add(offset as i16)) as u16
    }

    pub fn set_pc_offset(&mut self, offset: i8) {
        self.pc = self.get_pc_offset(offset);
    }

    pub fn get_pc(&self) -> u16 {
        self.pc
    }

    pub fn set_pc(&mut self, value: u16) {
        self.pc = value;
    }

    // While this functionality could be achieved with `self.set_pc_offset`, this seems like it
    // ought to be faster since it doesn't do any type casting. Need to make sure that's actually
    // the case.
    pub fn inc_pc(&mut self, amount: u16) {
        self.pc = self.pc.wrapping_add(amount);
    }
}