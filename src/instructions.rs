use crate::address_bus::AddressBus;
use crate::registers::{Flag, Reg8, Registers};
use std::fmt::{self, Display, Formatter};

pub trait Instruction {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus);
    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String;
}

pub struct NOP;

impl Instruction for NOP {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1)
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "NOP".to_string()
    }
}

pub struct DisableInterrupts;

impl Instruction for DisableInterrupts {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        reg.set_ime(false);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "DI".to_string()
    }
}

#[derive(Copy, Clone)]
pub enum Direction {
    ToBus,
    ToRegister,
}

pub struct Load8BitImmediate {
    pub reg: Reg8,
}

impl Instruction for Load8BitImmediate {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let val = bus.read_8bit(reg.get_pc_offset(1));
        reg.inc_pc(2);
        reg.set_8bit(self.reg, val);
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let val = bus.read_8bit(addr.wrapping_add(1));
        format!("LD {},${:02x}", self.reg, val)
    }
}

pub struct LoadHigh {
    pub direction: Direction,
}

impl Instruction for LoadHigh {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let addr = 0xff00 + (bus.read_8bit(reg.get_pc_offset(1)) as u16);
        reg.inc_pc(2);
        match self.direction {
            Direction::ToBus => {
                let val = reg.get_8bit(Reg8::A);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                reg.set_8bit(Reg8::A, val);
            }
        }
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let offset = bus.read_8bit(addr.wrapping_add(1));
        match self.direction {
            Direction::ToBus => format!("LDH (${:02x}),A", offset),
            Direction::ToRegister => format!("LDH A,(${:02x})", offset),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Condition {
    Z,
    NZ,
    C,
    NC,
}

impl Condition {
    fn is_met(self, reg: &Registers) -> bool {
        match self {
            Condition::Z => reg.get_flag(Flag::Z) == true,
            Condition::NZ => reg.get_flag(Flag::Z) == false,
            Condition::C => reg.get_flag(Flag::C) == true,
            Condition::NC => reg.get_flag(Flag::C) == false,
        }
    }
}

impl Display for Condition {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Condition::Z => "Z",
                Condition::NZ => "NZ",
                Condition::C => "C",
                Condition::NC => "NZ",
            }
        )
    }
}

pub struct JumpImmediate {
    pub condition: Option<Condition>,
}

impl Instruction for JumpImmediate {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let addr = bus.read_16bit(reg.get_pc_offset(1));
        reg.inc_pc(3);
        if self.condition.map_or(true, |c| c.is_met(reg)) {
            reg.set_pc(addr);
        }
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let addr = bus.read_16bit(addr.wrapping_add(1));
        match self.condition {
            None => format!("JP ${:04x}", addr),
            Some(cond) => format!("JP {},${:04x}", cond, addr),
        }
    }
}

pub struct JumpRelative {
    pub condition: Option<Condition>,
}

impl Instruction for JumpRelative {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let offset = bus.read_8bit(reg.get_pc_offset(1)) as i8;
        reg.inc_pc(2);
        if self.condition.map_or(true, |c| c.is_met(reg)) {
            reg.set_pc_offset(offset);
        }
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let offset = bus.read_8bit(addr.wrapping_add(1)) as i8;
        let formatted_offset = if offset > 0 {
            format!("+{:02x}", offset.abs())
        } else {
            format!("-{:02x}", offset.abs())
        };

        match self.condition {
            None => format!("JR ${}", formatted_offset),
            Some(cond) => format!("JR {},${}", cond, formatted_offset),
        }
    }
}

pub struct Return {
    pub condition: Option<Condition>,
}

impl Instruction for Return {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        reg.inc_pc(1);
        if self.condition.map_or(true, |c| c.is_met(reg)) {
            reg.set_pc(bus.read_16bit(reg.get_sp()));
            reg.set_sp_offset(2);
        }
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        match self.condition {
            None => "RET".to_string(),
            Some(cond) => format!("RET {}", cond),
        }
    }
}

pub struct ReturnInterrupt;

impl Instruction for ReturnInterrupt {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        reg.inc_pc(1);
        reg.set_ime(true);
        reg.set_pc(bus.read_16bit(reg.get_sp()));
        reg.set_sp_offset(2);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RETI".to_string()
    }
}

pub struct CompareImmediate;

impl Instruction for CompareImmediate {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let acc = reg.get_8bit(Reg8::A);
        let val = bus.read_8bit(reg.get_pc_offset(1));
        reg.inc_pc(2);
        reg.set_flag(Flag::Z, acc == val);
        reg.set_flag(Flag::N, true);
        reg.set_flag(Flag::H, (acc & 0x0f) < (val & 0x0f));
        reg.set_flag(Flag::C, acc < val);
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let val = bus.read_8bit(addr.wrapping_add(1));
        format!("CP ${:02x}", val)
    }
}

pub struct ExclusiveOr {
    pub reg: Reg8,
}

impl Instruction for ExclusiveOr {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        let acc = reg.get_8bit(Reg8::A);
        let val = reg.get_8bit(self.reg);
        reg.inc_pc(1);
        reg.set_8bit(Reg8::A, acc ^ val);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("XOR {}", self.reg)
    }
}
