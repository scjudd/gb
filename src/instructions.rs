use crate::bus::Address;
use crate::cpu::Compute;
use crate::registers::{Flag, Reg16, Reg8};
use std::fmt::{self, Display, Formatter};

pub trait Instruction {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address);
    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String;
}

pub struct NOP;

impl Instruction for NOP {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "NOP".to_string()
    }
}

pub struct DisableInterrupts;

impl Instruction for DisableInterrupts {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.set_ime(false);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "DI".to_string()
    }
}

pub struct EnableInterrupts;

impl Instruction for EnableInterrupts {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.set_ime(true);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "EI".to_string()
    }
}

pub struct Stop;

impl Instruction for Stop {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 2);
        cpu.set_stopped(true);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "STOP".to_string()
    }
}

pub struct RotateLeftCircularAccumulator;

impl Instruction for RotateLeftCircularAccumulator {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg8(Reg8::A);
        let rotated_bit = (last & 0b1000_0000) >> 7;
        let val = (last << 1) | rotated_bit;
        cpu.set_reg8(Reg8::A, val);
        cpu.set_flag(Flag::Z, false);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, false);
        cpu.set_flag(Flag::C, rotated_bit == 1);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "RLCA".to_string()
    }
}

pub struct RotateLeftAccumulator;

impl Instruction for RotateLeftAccumulator {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg8(Reg8::A);
        let carry = (last & 0b1000_0000) >> 7;
        let rotated_bit = cpu.get_flag(Flag::C) as u8;
        let val = (last << 1) | rotated_bit;
        cpu.set_reg8(Reg8::A, val);
        cpu.set_flag(Flag::Z, false);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, false);
        cpu.set_flag(Flag::C, carry == 1);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "RLA".to_string()
    }
}

pub struct RotateRightCircularAccumulator;

impl Instruction for RotateRightCircularAccumulator {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg8(Reg8::A);
        let rotated_bit = last & 0b0000_0001;
        let val = (last >> 1) | (rotated_bit << 7);
        cpu.set_reg8(Reg8::A, val);
        cpu.set_flag(Flag::Z, false);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, false);
        cpu.set_flag(Flag::C, rotated_bit == 1);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "RRCA".to_string()
    }
}

pub struct RotateRightAccumulator;

impl Instruction for RotateRightAccumulator {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg8(Reg8::A);
        let carry = last & 0b0000_0001;
        let rotated_bit = cpu.get_flag(Flag::C) as u8;
        let val = (last >> 1) | (rotated_bit << 7);
        cpu.set_reg8(Reg8::A, val);
        cpu.set_flag(Flag::Z, false);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, false);
        cpu.set_flag(Flag::C, carry == 1);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
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
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let val = cpu.get_reg8(self.src);
        cpu.set_reg8(self.dst, val);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("LD {},{}", self.dst, self.src)
    }
}

pub struct Load8BitImmediate {
    pub reg: Reg8,
}

impl Instruction for Load8BitImmediate {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let val = bus.read_8bit(cpu.get_reg16_offset(Reg16::PC, 1));
        cpu.set_reg16_offset(Reg16::PC, 2);
        cpu.set_reg8(self.reg, val);
        cpu.increment_cycle_count(8);
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
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
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let addr = cpu.get_reg16(self.addr_reg);
        cpu.set_reg16_offset(Reg16::PC, 1);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.get_reg8(self.reg);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.set_reg8(self.reg, val);
            }
        }
        cpu.increment_cycle_count(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        match self.direction {
            Direction::ToBus => format!("LD ({}),{}", self.addr_reg, self.reg),
            Direction::ToRegister => format!("LD {},({})", self.reg, self.addr_reg),
        }
    }
}

pub struct Load8BitIndirectIncrement {
    pub direction: Direction,
    pub addr_reg: Reg16,
    pub reg: Reg8,
}

impl Instruction for Load8BitIndirectIncrement {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let addr = cpu.get_reg16(self.addr_reg);
        cpu.set_reg16_offset(Reg16::PC, 1);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.get_reg8(self.reg);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.set_reg8(self.reg, val);
            }
        }
        cpu.set_reg16(self.addr_reg, addr.wrapping_add(1));
        cpu.increment_cycle_count(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        match self.direction {
            Direction::ToBus => format!("LD ({}+),{}", self.addr_reg, self.reg),
            Direction::ToRegister => format!("LD {},({}+)", self.reg, self.addr_reg),
        }
    }
}

pub struct Load8BitIndirectDecrement {
    pub direction: Direction,
    pub addr_reg: Reg16,
    pub reg: Reg8,
}

impl Instruction for Load8BitIndirectDecrement {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let addr = cpu.get_reg16(self.addr_reg);
        cpu.set_reg16_offset(Reg16::PC, 1);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.get_reg8(self.reg);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.set_reg8(self.reg, val);
            }
        }
        cpu.set_reg16(self.addr_reg, addr.wrapping_sub(1));
        cpu.increment_cycle_count(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        match self.direction {
            Direction::ToBus => format!("LD ({}-),{}", self.addr_reg, self.reg),
            Direction::ToRegister => format!("LD {},({}-)", self.reg, self.addr_reg),
        }
    }
}

pub struct LoadHigh {
    pub direction: Direction,
}

impl Instruction for LoadHigh {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let addr = 0xff00 + (bus.read_8bit(cpu.get_reg16_offset(Reg16::PC, 1)) as u16);
        cpu.set_reg16_offset(Reg16::PC, 2);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.get_reg8(Reg8::A);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.set_reg8(Reg8::A, val);
            }
        }
        cpu.increment_cycle_count(12);
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
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
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let val = bus.read_16bit(cpu.get_reg16_offset(Reg16::PC, 1));
        cpu.set_reg16_offset(Reg16::PC, 3);
        cpu.set_reg16(self.reg, val);
        cpu.increment_cycle_count(12);
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
        let val = bus.read_16bit(addr.wrapping_add(1));
        format!("LD {},${:04x}", self.reg, val)
    }
}

pub struct Load16BitIndirectImmediate;

impl Instruction for Load16BitIndirectImmediate {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let addr = bus.read_16bit(cpu.get_reg16_offset(Reg16::PC, 1));
        cpu.set_reg16_offset(Reg16::PC, 3);
        let val = cpu.get_reg16(Reg16::SP);
        bus.write_16bit(addr, val);
        cpu.increment_cycle_count(20);
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
        let addr = bus.read_16bit(addr.wrapping_add(1));
        format!("LD (${:04x}),SP", addr)
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
    fn is_met(self, cpu: &mut dyn Compute) -> bool {
        match self {
            Condition::Z => cpu.get_flag(Flag::Z) == true,
            Condition::NZ => cpu.get_flag(Flag::Z) == false,
            Condition::C => cpu.get_flag(Flag::C) == true,
            Condition::NC => cpu.get_flag(Flag::C) == false,
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
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let addr = bus.read_16bit(cpu.get_reg16_offset(Reg16::PC, 1));
        cpu.set_reg16_offset(Reg16::PC, 3);
        cpu.increment_cycle_count(12);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.set_reg16(Reg16::PC, addr);
            cpu.increment_cycle_count(4);
        }
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
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
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let offset = bus.read_8bit(cpu.get_reg16_offset(Reg16::PC, 1)) as i8;
        cpu.set_reg16_offset(Reg16::PC, 2);
        cpu.increment_cycle_count(8);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.set_reg16_offset(Reg16::PC, offset);
            cpu.increment_cycle_count(4);
        }
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
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
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let addr = bus.read_16bit(cpu.get_reg16_offset(Reg16::PC, 1));
        cpu.set_reg16_offset(Reg16::PC, 3);
        cpu.increment_cycle_count(12);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.set_reg16_offset(Reg16::SP, -2);
            bus.write_16bit(cpu.get_reg16(Reg16::SP), cpu.get_reg16(Reg16::PC));
            cpu.set_reg16(Reg16::PC, addr);
            cpu.increment_cycle_count(12);
        }
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
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
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.increment_cycle_count(8);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.set_reg16(Reg16::PC, bus.read_16bit(cpu.get_reg16(Reg16::SP)));
            cpu.set_reg16_offset(Reg16::SP, 2);
            cpu.increment_cycle_count(match self.condition {
                None => 8,
                Some(_) => 12,
            });
        }
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        match self.condition {
            None => "RET".to_string(),
            Some(cond) => format!("RET {}", cond),
        }
    }
}

pub struct ReturnInterrupt;

impl Instruction for ReturnInterrupt {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.set_ime(true);
        cpu.set_reg16(Reg16::PC, bus.read_16bit(cpu.get_reg16(Reg16::SP)));
        cpu.set_reg16_offset(Reg16::SP, 2);
        cpu.increment_cycle_count(16);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        "RETI".to_string()
    }
}

pub struct CompareImmediate;

impl Instruction for CompareImmediate {
    fn execute(&self, cpu: &mut dyn Compute, bus: &mut dyn Address) {
        let acc = cpu.get_reg8(Reg8::A);
        let val = bus.read_8bit(cpu.get_reg16_offset(Reg16::PC, 1));
        cpu.set_reg16_offset(Reg16::PC, 2);
        cpu.set_flag(Flag::Z, acc == val);
        cpu.set_flag(Flag::N, true);
        cpu.set_flag(Flag::H, (acc & 0x0f) < (val & 0x0f));
        cpu.set_flag(Flag::C, acc < val);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, addr: u16, bus: &dyn Address) -> String {
        let val = bus.read_8bit(addr.wrapping_add(1));
        format!("CP ${:02x}", val)
    }
}

pub struct Add {
    pub reg: Reg8,
}

impl Instruction for Add {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        let acc = cpu.get_reg8(Reg8::A);
        let amount = cpu.get_reg8(self.reg);
        let (val, carry) = acc.overflowing_add(amount);
        let halfcarry = ((acc & 0x0f) + (amount & 0x0f)) & 0x10 == 0x10;
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.set_reg8(Reg8::A, val);
        cpu.set_flag(Flag::Z, val == 0);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, halfcarry);
        cpu.set_flag(Flag::C, carry);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("ADD {}", self.reg)
    }
}

pub struct AddWithCarry {
    pub reg: Reg8,
}

impl Instruction for AddWithCarry {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let acc = cpu.get_reg8(Reg8::A);
        let (amount, carry) = cpu
            .get_reg8(self.reg)
            .overflowing_add(match cpu.get_flag(Flag::C) {
                true => 1,
                false => 0,
            });
        let (val, carry) = match carry {
            true => (acc.wrapping_add(amount), true),
            false => acc.overflowing_add(amount),
        };
        let halfcarry = ((acc & 0x0f) + (amount & 0x0f)) & 0x10 == 0x10;
        cpu.set_reg8(Reg8::A, val);
        cpu.set_flag(Flag::Z, val == 0);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, halfcarry);
        cpu.set_flag(Flag::C, carry);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("ADC {}", self.reg)
    }
}

pub struct ExclusiveOr {
    pub reg: Reg8,
}

impl Instruction for ExclusiveOr {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        let acc = cpu.get_reg8(Reg8::A);
        let val = acc ^ cpu.get_reg8(self.reg);
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.set_reg8(Reg8::A, val);
        cpu.set_flag(Flag::Z, val == 0);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, false);
        cpu.set_flag(Flag::C, false);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("XOR {}", self.reg)
    }
}

pub struct Increment8Bit {
    pub reg: Reg8,
}

impl Instruction for Increment8Bit {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg8(self.reg);
        let val = last.wrapping_sub(1);
        cpu.set_reg8(self.reg, val);
        cpu.set_flag(Flag::Z, val == 0);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, ((last & 0x0f) + (val & 0x0f)) & 0x10 == 0x10);
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("INC {}", self.reg)
    }
}

pub struct Decrement8Bit {
    pub reg: Reg8,
}

impl Instruction for Decrement8Bit {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg8(self.reg);
        let val = last.wrapping_sub(1);
        cpu.set_reg8(self.reg, val);
        cpu.set_flag(Flag::Z, val == 0);
        cpu.set_flag(Flag::N, true);
        cpu.set_flag(Flag::H, (last & 0x0f) < (val & 0x0f));
        cpu.increment_cycle_count(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("DEC {}", self.reg)
    }
}

pub struct Add16Bit {
    pub reg: Reg16,
}

impl Instruction for Add16Bit {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        let acc = cpu.get_reg16(Reg16::HL);
        let amount = cpu.get_reg16(self.reg);
        let (val, carry) = acc.overflowing_add(amount);
        let halfcarry = ((acc & 0x00ff) + (amount & 0x00ff)) & 0x0100 == 0x0100;
        cpu.set_reg16_offset(Reg16::PC, 1);
        cpu.set_reg16(Reg16::HL, val);
        cpu.set_flag(Flag::N, false);
        cpu.set_flag(Flag::H, halfcarry);
        cpu.set_flag(Flag::C, carry);
        cpu.increment_cycle_count(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("ADD HL,{}", self.reg)
    }
}

pub struct Increment16Bit {
    pub reg: Reg16,
}

impl Instruction for Increment16Bit {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg16(self.reg);
        let val = last.wrapping_add(1);
        cpu.set_reg16(self.reg, val);
        cpu.increment_cycle_count(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("INC {}", self.reg)
    }
}

pub struct Decrement16Bit {
    pub reg: Reg16,
}

impl Instruction for Decrement16Bit {
    fn execute(&self, cpu: &mut dyn Compute, _bus: &mut dyn Address) {
        cpu.set_reg16_offset(Reg16::PC, 1);
        let last = cpu.get_reg16(self.reg);
        let val = last.wrapping_sub(1);
        cpu.set_reg16(self.reg, val);
        cpu.increment_cycle_count(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &dyn Address) -> String {
        format!("DEC {}", self.reg)
    }
}

#[cfg(test)]
mod tests {
    //! Instruction tests should follow this general format:
    //!
    //! ```
    //! #[test]
    //! fn test_foo() {
    //!     let inst = &...;
    //!
    //!     let mut cpu = CpuSpy::wrap(Cpu::reset());
    //!     // ...
    //!     cpu.record_changes();
    //!
    //!     let mut bus = BusSpy::wrap(FakeBus::new());
    //!     // ...
    //!     bus.record_changes();
    //!
    //!     let mnemonic = inst.mnemonic(0x0000, &bus);
    //!     assert_eq!(mnemonic, "...");
    //!
    //!     inst.execute(&mut cpu, &mut bus);
    //!     cpu.assert_expectations();
    //!     bus.assert_expectations();
    //! }
    //! ```

    use super::*;
    use crate::bus::FakeBus;
    use crate::cpu::Cpu;
    use crate::spies::{BusSpy, CpuSpy};

    #[test]
    fn test_nop() {
        let inst = &NOP;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "NOP");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_disable_interrupts() {
        let inst = &DisableInterrupts;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_ime(false);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "DI");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_enable_interrupts() {
        let inst = &EnableInterrupts;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_ime(true);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "EI");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_stop() {
        let inst = &Stop;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_stop(true);
        cpu.expect_reg16(Reg16::PC, 0x0002);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "STOP");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_rotate_left_circular_accumulator() {
        let inst = &RotateLeftCircularAccumulator;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg8(Reg8::A, 0b1000_0000);
        cpu.expect_reg8(Reg8::A, 0b0000_0001);
        cpu.expect_flag(Flag::Z, false);
        cpu.expect_flag(Flag::N, false);
        cpu.expect_flag(Flag::H, false);
        cpu.expect_flag(Flag::C, true);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "RLCA");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_rotate_left_accumulator() {
        let inst = &RotateLeftAccumulator;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg8(Reg8::A, 0b0000_0000);
        cpu.set_flag(Flag::C, true);
        cpu.expect_reg8(Reg8::A, 0b0000_0001);
        cpu.expect_flag(Flag::Z, false);
        cpu.expect_flag(Flag::N, false);
        cpu.expect_flag(Flag::H, false);
        cpu.expect_flag(Flag::C, false);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "RLA");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_rotate_right_circular_accumulator() {
        let inst = &RotateRightCircularAccumulator;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg8(Reg8::A, 0b0000_0001);
        cpu.expect_reg8(Reg8::A, 0b1000_0000);
        cpu.expect_flag(Flag::Z, false);
        cpu.expect_flag(Flag::N, false);
        cpu.expect_flag(Flag::H, false);
        cpu.expect_flag(Flag::C, true);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "RRCA");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_rotate_right_accumulator() {
        let inst = &RotateRightAccumulator;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg8(Reg8::A, 0b0000_0000);
        cpu.set_flag(Flag::C, true);
        cpu.expect_reg8(Reg8::A, 0b1000_0000);
        cpu.expect_flag(Flag::Z, false);
        cpu.expect_flag(Flag::N, false);
        cpu.expect_flag(Flag::H, false);
        cpu.expect_flag(Flag::C, false);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "RRA");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit() {
        let inst = &Load8Bit {
            dst: Reg8::A,
            src: Reg8::B,
        };

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg8(Reg8::A, 0x00);
        cpu.set_reg8(Reg8::B, 0xff);
        cpu.expect_reg8(Reg8::A, 0xff);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(4);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "LD A,B");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit_immediate() {
        let inst = &Load8BitImmediate { reg: Reg8::A };

        let value = 0xaa;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::PC, 0x0002);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(0x0001, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("LD A,${:02x}", value));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit_indirect_to_bus() {
        let inst = &Load8BitIndirect {
            direction: Direction::ToBus,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };

        let addr = 0xc000;
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::BC, addr);
        cpu.set_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.expect_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "LD (BC),A");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit_indirect_to_register() {
        let inst = &Load8BitIndirect {
            direction: Direction::ToRegister,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };

        let addr = 0xc000;
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::BC, addr);
        cpu.expect_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "LD A,(BC)");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit_indirect_increment_to_bus() {
        let inst = &Load8BitIndirectIncrement {
            direction: Direction::ToBus,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };

        let addr = 0xc000;
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::BC, addr);
        cpu.set_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::BC, addr.wrapping_add(1));
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.expect_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "LD (BC+),A");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit_indirect_increment_to_register() {
        let inst = &Load8BitIndirectIncrement {
            direction: Direction::ToRegister,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };

        let addr = 0xc000;
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::BC, addr);
        cpu.expect_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::BC, addr.wrapping_add(1));
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "LD A,(BC+)");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit_indirect_decrement_to_bus() {
        let inst = &Load8BitIndirectDecrement {
            direction: Direction::ToBus,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };

        let addr = 0xc000;
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::BC, addr);
        cpu.set_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::BC, addr.wrapping_sub(1));
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.expect_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "LD (BC-),A");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_8bit_indirect_decrement_to_register() {
        let inst = &Load8BitIndirectDecrement {
            direction: Direction::ToRegister,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };

        let addr = 0xc000;
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::BC, addr);
        cpu.expect_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::BC, addr.wrapping_sub(1));
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "LD A,(BC-)");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_high_to_bus() {
        let inst = &LoadHigh {
            direction: Direction::ToBus,
        };

        let offset = 0xaa;
        let addr = 0xff00_u16.wrapping_add(offset as u16);
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::PC, 0x0002);
        cpu.expect_cycles(12);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(0x0001, offset);
        bus.expect_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("LDH (${:02x}),A", offset));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_high_to_register() {
        let inst = &LoadHigh {
            direction: Direction::ToRegister,
        };

        let offset = 0xaa;
        let addr = 0xff00_u16.wrapping_add(offset as u16);
        let value = 0xff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_reg8(Reg8::A, value);
        cpu.expect_reg16(Reg16::PC, 0x0002);
        cpu.expect_cycles(12);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(0x0001, offset);
        bus.write_8bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("LDH A,(${:02x})", offset));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_16bit_immediate() {
        let inst = &Load16BitImmediate { reg: Reg16::BC };

        let value = 0xffff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_reg16(Reg16::BC, value);
        cpu.expect_reg16(Reg16::PC, 0x0003);
        cpu.expect_cycles(12);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("LD BC,${:04x}", value));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_load_16bit_indirect_immediate() {
        let inst = &Load16BitIndirectImmediate;

        let addr = 0xc6aa;
        let value = 0xffff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::SP, value);
        cpu.expect_reg16(Reg16::PC, 0x0003);
        cpu.expect_cycles(20);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, addr);
        bus.expect_16bit(addr, value);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("LD (${:04x}),SP", addr));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_jump_immediate_no_condition() {
        let inst = &JumpImmediate { condition: None };

        let addr = 0xc6aa;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.expect_reg16(Reg16::PC, addr);
        cpu.expect_cycles(16);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, addr);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("JP ${:04x}", addr));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_jump_immediate_condition_met() {
        let inst = &JumpImmediate {
            condition: Some(Condition::Z),
        };

        let addr = 0xc6aa;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_flag(Flag::Z, true);
        cpu.expect_reg16(Reg16::PC, addr);
        cpu.expect_cycles(16);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, addr);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("JP Z,${:04x}", addr));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_jump_immediate_condition_unmet() {
        let inst = &JumpImmediate {
            condition: Some(Condition::Z),
        };

        let addr = 0xc6aa;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_flag(Flag::Z, false);
        cpu.expect_reg16(Reg16::PC, 0x0003);
        cpu.expect_cycles(12);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, addr);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("JP Z,${:04x}", addr));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_jump_relative_no_condition() {
        let inst = &JumpRelative { condition: None };

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::PC, 0x0100);
        cpu.expect_reg16(Reg16::PC, 0x00f2);
        cpu.expect_cycles(12);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(0x0101, 0xf0);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0100, &bus);
        assert_eq!(mnemonic, "JR $-10");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_jump_relative_condition_met() {
        let inst = &JumpRelative {
            condition: Some(Condition::Z),
        };

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_flag(Flag::Z, true);
        cpu.set_reg16(Reg16::PC, 0x0100);
        cpu.expect_reg16(Reg16::PC, 0x00f2);
        cpu.expect_cycles(12);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(0x0101, 0xf0);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0100, &bus);
        assert_eq!(mnemonic, "JR Z,$-10");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_jump_relative_condition_unmet() {
        let inst = &JumpRelative {
            condition: Some(Condition::Z),
        };

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_flag(Flag::Z, false);
        cpu.set_reg16(Reg16::PC, 0x0100);
        cpu.expect_reg16(Reg16::PC, 0x0102);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_8bit(0x0101, 0xf0);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0100, &bus);
        assert_eq!(mnemonic, "JR Z,$-10");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_call_no_condition() {
        let inst = &Call { condition: None };

        let jump_addr = 0x0100;
        let return_addr = 0x0003;
        let old_stack_pointer: u16 = 0xc6ff;
        let new_stack_pointer = old_stack_pointer.wrapping_sub(2);

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::SP, old_stack_pointer);
        cpu.expect_reg16(Reg16::SP, new_stack_pointer);
        cpu.expect_reg16(Reg16::PC, jump_addr);
        cpu.expect_cycles(24);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, jump_addr);
        bus.expect_16bit(new_stack_pointer, return_addr);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("CALL ${:04x}", jump_addr));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_call_condition_met() {
        let inst = &Call {
            condition: Some(Condition::Z),
        };

        let jump_addr = 0x0100;
        let return_addr = 0x0003;
        let old_stack_pointer: u16 = 0xc6ff;
        let new_stack_pointer = old_stack_pointer.wrapping_sub(2);

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::SP, old_stack_pointer);
        cpu.set_flag(Flag::Z, true);
        cpu.expect_reg16(Reg16::SP, new_stack_pointer);
        cpu.expect_reg16(Reg16::PC, jump_addr);
        cpu.expect_cycles(24);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, jump_addr);
        bus.expect_16bit(new_stack_pointer, return_addr);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("CALL Z,${:04x}", jump_addr));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_call_condition_unmet() {
        let inst = &Call {
            condition: Some(Condition::Z),
        };

        let jump_addr = 0x0100;
        let stack_pointer: u16 = 0xc6ff;

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::SP, stack_pointer);
        cpu.expect_reg16(Reg16::PC, 0x0003);
        cpu.expect_cycles(12);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0x0001, jump_addr);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, format!("CALL Z,${:04x}", jump_addr));

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_return_no_condition() {
        let inst = &Return { condition: None };

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_reg16(Reg16::SP, 0xc6fd);
        cpu.expect_reg16(Reg16::SP, 0xc6ff);
        cpu.expect_reg16(Reg16::PC, 0x0100);
        cpu.expect_cycles(16);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0xc6fd, 0x0100);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "RET");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_return_condition_met() {
        let inst = &Return {
            condition: Some(Condition::Z),
        };

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_flag(Flag::Z, true);
        cpu.set_reg16(Reg16::SP, 0xc6fd);
        cpu.expect_reg16(Reg16::SP, 0xc6ff);
        cpu.expect_reg16(Reg16::PC, 0x0100);
        cpu.expect_cycles(20);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0xc6fd, 0x0100);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "RET Z");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }

    #[test]
    fn test_return_condition_unmet() {
        let inst = &Return {
            condition: Some(Condition::Z),
        };

        let mut cpu = CpuSpy::wrap(Cpu::reset());
        cpu.set_flag(Flag::Z, false);
        cpu.expect_reg16(Reg16::PC, 0x0001);
        cpu.expect_cycles(8);
        cpu.record_changes();

        let mut bus = BusSpy::wrap(FakeBus::new());
        bus.write_16bit(0xc6fd, 0x0100);
        bus.record_changes();

        let mnemonic = inst.mnemonic(0x0000, &bus);
        assert_eq!(mnemonic, "RET Z");

        inst.execute(&mut cpu, &mut bus);
        cpu.assert_expectations();
        bus.assert_expectations();
    }
}
