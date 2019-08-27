use crate::address_bus::AddressBus;
use crate::registers::{Flag, Reg16, Reg8, Registers};
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

pub struct EnableInterrupts;

impl Instruction for EnableInterrupts {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        reg.set_ime(true);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "EI".to_string()
    }
}

pub struct Stop;

impl Instruction for Stop {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(2);
        reg.set_stopped(true);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "STOP".to_string()
    }
}

pub struct RotateLeftCircularAccumulator;

impl Instruction for RotateLeftCircularAccumulator {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_8bit(Reg8::A);
        let rotated_bit = (last & 0b1000_0000) >> 7;
        let val = (last << 1) & rotated_bit;
        reg.set_8bit(Reg8::A, val);
        reg.set_flag(Flag::Z, false);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, false);
        reg.set_flag(Flag::C, rotated_bit == 1);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RLCA".to_string()
    }
}

pub struct RotateLeftAccumulator;

impl Instruction for RotateLeftAccumulator {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_8bit(Reg8::A);
        let carry = (last & 0b1000_0000) >> 7;
        let rotated_bit = match reg.get_flag(Flag::C) {
            true => 1,
            false => 0,
        };
        let val = (last << 1) & rotated_bit;
        reg.set_8bit(Reg8::A, val);
        reg.set_flag(Flag::Z, false);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, false);
        reg.set_flag(Flag::C, carry == 1);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RLA".to_string()
    }
}

pub struct RotateRightCircularAccumulator;

impl Instruction for RotateRightCircularAccumulator {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_8bit(Reg8::A);
        let rotated_bit = last & 0b0000_0001;
        let val = (last >> 1) & (rotated_bit << 7);
        reg.set_8bit(Reg8::A, val);
        reg.set_flag(Flag::Z, false);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, false);
        reg.set_flag(Flag::C, rotated_bit == 1);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RRCA".to_string()
    }
}

pub struct RotateRightAccumulator;

impl Instruction for RotateRightAccumulator {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_8bit(Reg8::A);
        let carry = last & 0b0000_0001;
        let rotated_bit = match reg.get_flag(Flag::C) {
            true => 1,
            false => 0,
        };
        let val = (last >> 1) & (rotated_bit << 7);
        reg.set_8bit(Reg8::A, val);
        reg.set_flag(Flag::Z, false);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, false);
        reg.set_flag(Flag::C, carry == 1);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RRA".to_string()
    }
}

#[derive(Copy, Clone)]
pub enum Direction {
    ToBus,
    ToRegister,
}

pub struct Load8Bit {
    pub dst: Reg8,
    pub src: Reg8,
}

impl Instruction for Load8Bit {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(2);
        let val = reg.get_8bit(self.src);
        reg.set_8bit(self.dst, val);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("LD {},{}", self.dst, self.src)
    }
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

pub struct Load8BitIndirect {
    pub direction: Direction,
    pub addr_reg: Reg16,
    pub reg: Reg8,
}

impl Instruction for Load8BitIndirect {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let addr = reg.get_16bit(self.addr_reg);
        reg.inc_pc(2);
        match self.direction {
            Direction::ToBus => {
                let val = reg.get_8bit(self.reg);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                reg.set_8bit(self.reg, val);
            }
        }
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        match self.direction {
            Direction::ToBus => format!("LD ({}),{}", self.addr_reg, self.reg),
            Direction::ToRegister => format!("LD {},({})", self.reg, self.addr_reg),
        }
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

pub struct Load16BitImmediate {
    pub reg: Reg16,
}

impl Instruction for Load16BitImmediate {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let val = bus.read_16bit(reg.get_pc_offset(1));
        reg.inc_pc(3);
        reg.set_16bit(self.reg, val);
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let val = bus.read_16bit(addr.wrapping_add(1));
        format!("LD {},${:04x}", self.reg, val)
    }
}

pub struct Load16BitIndirectImmediate {
    pub direction: Direction,
    pub reg: Reg16,
}

impl Instruction for Load16BitIndirectImmediate {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let addr = bus.read_16bit(reg.get_pc_offset(1));
        reg.inc_pc(3);
        match self.direction {
            Direction::ToBus => {
                let val = reg.get_16bit(self.reg);
                bus.write_16bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_16bit(addr);
                reg.set_16bit(self.reg, val);
            }
        };
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let addr = bus.read_16bit(addr.wrapping_add(1));
        match self.direction {
            Direction::ToBus => format!("LD (${:04x}),{}", addr, self.reg),
            Direction::ToRegister => format!("LD {},(${:04x})", self.reg, addr),
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

pub struct Call {
    pub condition: Option<Condition>,
}

impl Instruction for Call {
    fn execute(&self, reg: &mut Registers, bus: &mut AddressBus) {
        let addr = bus.read_16bit(reg.get_pc_offset(1));
        reg.inc_pc(3);
        if self.condition.map_or(true, |c| c.is_met(reg)) {
            reg.set_sp_offset(-2);
            bus.write_16bit(reg.get_sp(), reg.get_pc());
            reg.set_pc(addr);
        }
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let call_addr = bus.read_16bit(addr.wrapping_add(1));
        match self.condition {
            None => format!("CALL ${:04x}", call_addr),
            Some(cond) => format!("CALL {},${:04x}", cond, call_addr),
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

pub struct Add {
    pub reg: Reg8,
}

impl Instruction for Add {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        let acc = reg.get_8bit(Reg8::A);
        let amount = reg.get_8bit(self.reg);
        let (val, carry) = acc.overflowing_add(amount);
        let halfcarry = ((acc & 0x0f) + (amount & 0x0f)) & 0x10 == 0x10;
        reg.inc_pc(1);
        reg.set_8bit(Reg8::A, val);
        reg.set_flag(Flag::Z, val == 0);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, halfcarry);
        reg.set_flag(Flag::C, carry);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("ADD {}", self.reg)
    }
}

pub struct AddWithCarry {
    pub reg: Reg8,
}

impl Instruction for AddWithCarry {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let acc = reg.get_8bit(Reg8::A);
        let (amount, carry) = reg
            .get_8bit(self.reg)
            .overflowing_add(match reg.get_flag(Flag::C) {
                true => 1,
                false => 0,
            });
        let (val, carry) = match carry {
            true => (acc.wrapping_add(amount), true),
            false => acc.overflowing_add(amount),
        };
        let halfcarry = ((acc & 0x0f) + (amount & 0x0f)) & 0x10 == 0x10;
        reg.set_8bit(Reg8::A, val);
        reg.set_flag(Flag::Z, val == 0);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, halfcarry);
        reg.set_flag(Flag::C, carry);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("ADC {}", self.reg)
    }
}

pub struct ExclusiveOr {
    pub reg: Reg8,
}

impl Instruction for ExclusiveOr {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        let acc = reg.get_8bit(Reg8::A);
        let val = acc ^ reg.get_8bit(self.reg);
        reg.inc_pc(1);
        reg.set_8bit(Reg8::A, val);
        reg.set_flag(Flag::Z, val == 0);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, false);
        reg.set_flag(Flag::C, false);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("XOR {}", self.reg)
    }
}

pub struct Increment8Bit {
    pub reg: Reg8,
}

impl Instruction for Increment8Bit {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_8bit(self.reg);
        let val = last.wrapping_sub(1);
        reg.set_8bit(self.reg, val);
        reg.set_flag(Flag::Z, val == 0);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, ((last & 0x0f) + (val & 0x0f)) & 0x10 == 0x10);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("INC {}", self.reg)
    }
}

pub struct Decrement8Bit {
    pub reg: Reg8,
}

impl Instruction for Decrement8Bit {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_8bit(self.reg);
        let val = last.wrapping_sub(1);
        reg.set_8bit(self.reg, val);
        reg.set_flag(Flag::Z, val == 0);
        reg.set_flag(Flag::N, true);
        reg.set_flag(Flag::H, (last & 0x0f) < (val & 0x0f));
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("DEC {}", self.reg)
    }
}

pub struct Add16Bit {
    pub reg: Reg16,
}

impl Instruction for Add16Bit {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        let acc = reg.get_16bit(Reg16::HL);
        let amount = reg.get_16bit(self.reg);
        let (val, carry) = acc.overflowing_add(amount);
        let halfcarry = ((acc & 0x00ff) + (amount & 0x00ff)) & 0x0100 == 0x0100;
        reg.inc_pc(1);
        reg.set_16bit(Reg16::HL, val);
        reg.set_flag(Flag::N, false);
        reg.set_flag(Flag::H, halfcarry);
        reg.set_flag(Flag::C, carry);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("ADD HL,{}", self.reg)
    }
}

pub struct Increment16Bit {
    pub reg: Reg16,
}

impl Instruction for Increment16Bit {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_16bit(self.reg);
        let val = last.wrapping_add(1);
        reg.set_16bit(self.reg, val);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("INC {}", self.reg)
    }
}

pub struct Decrement16Bit {
    pub reg: Reg16,
}

impl Instruction for Decrement16Bit {
    fn execute(&self, reg: &mut Registers, _bus: &mut AddressBus) {
        reg.inc_pc(1);
        let last = reg.get_16bit(self.reg);
        let val = last.wrapping_sub(1);
        reg.set_16bit(self.reg, val);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("DEC {}", self.reg)
    }
}
