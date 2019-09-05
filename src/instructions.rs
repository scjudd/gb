use crate::address_bus::AddressBus;
use crate::cpu::CPU;
use crate::registers::{Flag, Reg16, Reg8};
use std::fmt::{self, Display, Formatter};

pub trait Instruction {
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus);
    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String;
}

pub struct NOP;

impl Instruction for NOP {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "NOP".to_string()
    }
}

pub struct DisableInterrupts;

impl Instruction for DisableInterrupts {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        cpu.ime = false;
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "DI".to_string()
    }
}

pub struct EnableInterrupts;

impl Instruction for EnableInterrupts {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        cpu.ime = true;
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "EI".to_string()
    }
}

pub struct Stop;

impl Instruction for Stop {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(2);
        cpu.stopped = true;
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "STOP".to_string()
    }
}

pub struct RotateLeftCircularAccumulator;

impl Instruction for RotateLeftCircularAccumulator {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_8bit(Reg8::A);
        let rotated_bit = (last & 0b1000_0000) >> 7;
        let val = (last << 1) | rotated_bit;
        cpu.reg.set_8bit(Reg8::A, val);
        cpu.reg.set_flag(Flag::Z, false);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, false);
        cpu.reg.set_flag(Flag::C, rotated_bit == 1);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RLCA".to_string()
    }
}

pub struct RotateLeftAccumulator;

impl Instruction for RotateLeftAccumulator {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_8bit(Reg8::A);
        let carry = (last & 0b1000_0000) >> 7;
        let rotated_bit = cpu.reg.get_flag(Flag::C) as u8;
        let val = (last << 1) | rotated_bit;
        cpu.reg.set_8bit(Reg8::A, val);
        cpu.reg.set_flag(Flag::Z, false);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, false);
        cpu.reg.set_flag(Flag::C, carry == 1);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RLA".to_string()
    }
}

pub struct RotateRightCircularAccumulator;

impl Instruction for RotateRightCircularAccumulator {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_8bit(Reg8::A);
        let rotated_bit = last & 0b0000_0001;
        let val = (last >> 1) | (rotated_bit << 7);
        cpu.reg.set_8bit(Reg8::A, val);
        cpu.reg.set_flag(Flag::Z, false);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, false);
        cpu.reg.set_flag(Flag::C, rotated_bit == 1);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RRCA".to_string()
    }
}

pub struct RotateRightAccumulator;

impl Instruction for RotateRightAccumulator {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_8bit(Reg8::A);
        let carry = last & 0b0000_0001;
        let rotated_bit = cpu.reg.get_flag(Flag::C) as u8;
        let val = (last >> 1) | (rotated_bit << 7);
        cpu.reg.set_8bit(Reg8::A, val);
        cpu.reg.set_flag(Flag::Z, false);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, false);
        cpu.reg.set_flag(Flag::C, carry == 1);
        cpu.inc_mtime(4);
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
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let val = cpu.reg.get_8bit(self.src);
        cpu.reg.set_8bit(self.dst, val);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("LD {},{}", self.dst, self.src)
    }
}

pub struct Load8BitImmediate {
    pub reg: Reg8,
}

impl Instruction for Load8BitImmediate {
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let val = bus.read_8bit(cpu.reg.get_pc_offset(1));
        cpu.reg.inc_pc(2);
        cpu.reg.set_8bit(self.reg, val);
        cpu.inc_mtime(8);
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let addr = cpu.reg.get_16bit(self.addr_reg);
        cpu.reg.inc_pc(1);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.reg.get_8bit(self.reg);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.reg.set_8bit(self.reg, val);
            }
        }
        cpu.inc_mtime(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let addr = cpu.reg.get_16bit(self.addr_reg);
        cpu.reg.inc_pc(1);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.reg.get_8bit(self.reg);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.reg.set_8bit(self.reg, val);
            }
        }
        cpu.reg.set_16bit(self.addr_reg, addr.wrapping_add(1));
        cpu.inc_mtime(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let addr = cpu.reg.get_16bit(self.addr_reg);
        cpu.reg.inc_pc(1);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.reg.get_8bit(self.reg);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.reg.set_8bit(self.reg, val);
            }
        }
        cpu.reg.set_16bit(self.addr_reg, addr.wrapping_sub(1));
        cpu.inc_mtime(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let addr = 0xff00 + (bus.read_8bit(cpu.reg.get_pc_offset(1)) as u16);
        cpu.reg.inc_pc(2);
        match self.direction {
            Direction::ToBus => {
                let val = cpu.reg.get_8bit(Reg8::A);
                bus.write_8bit(addr, val);
            }
            Direction::ToRegister => {
                let val = bus.read_8bit(addr);
                cpu.reg.set_8bit(Reg8::A, val);
            }
        }
        cpu.inc_mtime(12);
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let val = bus.read_16bit(cpu.reg.get_pc_offset(1));
        cpu.reg.inc_pc(3);
        cpu.reg.set_16bit(self.reg, val);
        cpu.inc_mtime(12);
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
        let val = bus.read_16bit(addr.wrapping_add(1));
        format!("LD {},${:04x}", self.reg, val)
    }
}

pub struct Load16BitIndirectImmediate;

impl Instruction for Load16BitIndirectImmediate {
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let addr = bus.read_16bit(cpu.reg.get_pc_offset(1));
        cpu.reg.inc_pc(3);
        let val = cpu.reg.get_16bit(Reg16::SP);
        bus.write_16bit(addr, val);
        cpu.inc_mtime(20);
    }

    fn mnemonic(&self, addr: u16, bus: &AddressBus) -> String {
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
    fn is_met(self, cpu: &CPU) -> bool {
        match self {
            Condition::Z => cpu.reg.get_flag(Flag::Z) == true,
            Condition::NZ => cpu.reg.get_flag(Flag::Z) == false,
            Condition::C => cpu.reg.get_flag(Flag::C) == true,
            Condition::NC => cpu.reg.get_flag(Flag::C) == false,
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let addr = bus.read_16bit(cpu.reg.get_pc_offset(1));
        cpu.reg.inc_pc(3);
        cpu.inc_mtime(12);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.reg.set_pc(addr);
            cpu.inc_mtime(4);
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let offset = bus.read_8bit(cpu.reg.get_pc_offset(1)) as i8;
        cpu.reg.inc_pc(2);
        cpu.inc_mtime(8);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.reg.set_pc_offset(offset);
            cpu.inc_mtime(4);
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let addr = bus.read_16bit(cpu.reg.get_pc_offset(1));
        cpu.reg.inc_pc(3);
        cpu.inc_mtime(12);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.reg.set_sp_offset(-2);
            bus.write_16bit(cpu.reg.get_sp(), cpu.reg.get_pc());
            cpu.reg.set_pc(addr);
            cpu.inc_mtime(12);
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        cpu.inc_mtime(8);
        if self.condition.map_or(true, |c| c.is_met(cpu)) {
            cpu.reg.set_pc(bus.read_16bit(cpu.reg.get_sp()));
            cpu.reg.set_sp_offset(2);
            cpu.inc_mtime(12);
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
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        cpu.ime = true;
        cpu.reg.set_pc(bus.read_16bit(cpu.reg.get_sp()));
        cpu.reg.set_sp_offset(2);
        cpu.inc_mtime(16);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        "RETI".to_string()
    }
}

pub struct CompareImmediate;

impl Instruction for CompareImmediate {
    fn execute(&self, cpu: &mut CPU, bus: &mut AddressBus) {
        let acc = cpu.reg.get_8bit(Reg8::A);
        let val = bus.read_8bit(cpu.reg.get_pc_offset(1));
        cpu.reg.inc_pc(2);
        cpu.reg.set_flag(Flag::Z, acc == val);
        cpu.reg.set_flag(Flag::N, true);
        cpu.reg.set_flag(Flag::H, (acc & 0x0f) < (val & 0x0f));
        cpu.reg.set_flag(Flag::C, acc < val);
        cpu.inc_mtime(4);
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
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        let acc = cpu.reg.get_8bit(Reg8::A);
        let amount = cpu.reg.get_8bit(self.reg);
        let (val, carry) = acc.overflowing_add(amount);
        let halfcarry = ((acc & 0x0f) + (amount & 0x0f)) & 0x10 == 0x10;
        cpu.reg.inc_pc(1);
        cpu.reg.set_8bit(Reg8::A, val);
        cpu.reg.set_flag(Flag::Z, val == 0);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, halfcarry);
        cpu.reg.set_flag(Flag::C, carry);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("ADD {}", self.reg)
    }
}

pub struct AddWithCarry {
    pub reg: Reg8,
}

impl Instruction for AddWithCarry {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let acc = cpu.reg.get_8bit(Reg8::A);
        let (amount, carry) =
            cpu.reg
                .get_8bit(self.reg)
                .overflowing_add(match cpu.reg.get_flag(Flag::C) {
                    true => 1,
                    false => 0,
                });
        let (val, carry) = match carry {
            true => (acc.wrapping_add(amount), true),
            false => acc.overflowing_add(amount),
        };
        let halfcarry = ((acc & 0x0f) + (amount & 0x0f)) & 0x10 == 0x10;
        cpu.reg.set_8bit(Reg8::A, val);
        cpu.reg.set_flag(Flag::Z, val == 0);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, halfcarry);
        cpu.reg.set_flag(Flag::C, carry);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("ADC {}", self.reg)
    }
}

pub struct ExclusiveOr {
    pub reg: Reg8,
}

impl Instruction for ExclusiveOr {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        let acc = cpu.reg.get_8bit(Reg8::A);
        let val = acc ^ cpu.reg.get_8bit(self.reg);
        cpu.reg.inc_pc(1);
        cpu.reg.set_8bit(Reg8::A, val);
        cpu.reg.set_flag(Flag::Z, val == 0);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, false);
        cpu.reg.set_flag(Flag::C, false);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("XOR {}", self.reg)
    }
}

pub struct Increment8Bit {
    pub reg: Reg8,
}

impl Instruction for Increment8Bit {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_8bit(self.reg);
        let val = last.wrapping_sub(1);
        cpu.reg.set_8bit(self.reg, val);
        cpu.reg.set_flag(Flag::Z, val == 0);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg
            .set_flag(Flag::H, ((last & 0x0f) + (val & 0x0f)) & 0x10 == 0x10);
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("INC {}", self.reg)
    }
}

pub struct Decrement8Bit {
    pub reg: Reg8,
}

impl Instruction for Decrement8Bit {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_8bit(self.reg);
        let val = last.wrapping_sub(1);
        cpu.reg.set_8bit(self.reg, val);
        cpu.reg.set_flag(Flag::Z, val == 0);
        cpu.reg.set_flag(Flag::N, true);
        cpu.reg.set_flag(Flag::H, (last & 0x0f) < (val & 0x0f));
        cpu.inc_mtime(4);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("DEC {}", self.reg)
    }
}

pub struct Add16Bit {
    pub reg: Reg16,
}

impl Instruction for Add16Bit {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        let acc = cpu.reg.get_16bit(Reg16::HL);
        let amount = cpu.reg.get_16bit(self.reg);
        let (val, carry) = acc.overflowing_add(amount);
        let halfcarry = ((acc & 0x00ff) + (amount & 0x00ff)) & 0x0100 == 0x0100;
        cpu.reg.inc_pc(1);
        cpu.reg.set_16bit(Reg16::HL, val);
        cpu.reg.set_flag(Flag::N, false);
        cpu.reg.set_flag(Flag::H, halfcarry);
        cpu.reg.set_flag(Flag::C, carry);
        cpu.inc_mtime(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("ADD HL,{}", self.reg)
    }
}

pub struct Increment16Bit {
    pub reg: Reg16,
}

impl Instruction for Increment16Bit {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_16bit(self.reg);
        let val = last.wrapping_add(1);
        cpu.reg.set_16bit(self.reg, val);
        cpu.inc_mtime(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("INC {}", self.reg)
    }
}

pub struct Decrement16Bit {
    pub reg: Reg16,
}

impl Instruction for Decrement16Bit {
    fn execute(&self, cpu: &mut CPU, _bus: &mut AddressBus) {
        cpu.reg.inc_pc(1);
        let last = cpu.reg.get_16bit(self.reg);
        let val = last.wrapping_sub(1);
        cpu.reg.set_16bit(self.reg, val);
        cpu.inc_mtime(8);
    }

    fn mnemonic(&self, _addr: u16, _bus: &AddressBus) -> String {
        format!("DEC {}", self.reg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixtures() -> (CPU, AddressBus) {
        let cpu = CPU::reset();
        let bus = AddressBus::from_raw(&[0x00, 0xaa, 0xc6]);
        (cpu, bus)
    }

    fn test_instruction(
        inst: &dyn Instruction,
        mut cpu: CPU,
        mut bus: AddressBus,
    ) -> (CPU, AddressBus, String) {
        let mnemonic = inst.mnemonic(0x0000, &mut bus);
        inst.execute(&mut cpu, &mut bus);
        (cpu, bus, mnemonic)
    }

    #[test]
    fn test_nop() {
        let (cpu, bus) = fixtures();
        let (cpu, _bus, mnemonic) = test_instruction(&NOP, cpu, bus);

        assert_eq!(mnemonic, "NOP");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 4);
    }

    #[test]
    fn test_disable_interrupts() {
        let (cpu, bus) = fixtures();
        let (cpu, _bus, mnemonic) = test_instruction(&DisableInterrupts, cpu, bus);

        assert_eq!(mnemonic, "DI");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.ime, false);
        assert_eq!(cpu.mtime, 4);
    }

    #[test]
    fn test_enable_interrupts() {
        let (mut cpu, bus) = fixtures();
        cpu.ime = false;
        let (cpu, _bus, mnemonic) = test_instruction(&EnableInterrupts, cpu, bus);

        assert_eq!(mnemonic, "EI");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.ime, true);
        assert_eq!(cpu.mtime, 4);
    }

    #[test]
    fn test_stop() {
        let (cpu, bus) = fixtures();
        let (cpu, _bus, mnemonic) = test_instruction(&Stop, cpu, bus);

        assert_eq!(mnemonic, "STOP");
        assert_eq!(cpu.reg.get_pc(), 2);
        assert_eq!(cpu.stopped, true);
        assert_eq!(cpu.mtime, 4);
    }

    #[test]
    fn test_rotate_left_circular_accumulator() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0b1000_0000);
        let (cpu, _bus, mnemonic) = test_instruction(&RotateLeftCircularAccumulator, cpu, bus);

        assert_eq!(mnemonic, "RLCA");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 4);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0b0000_0001);
        assert_eq!(cpu.reg.get_flag(Flag::C), true);
    }

    #[test]
    fn test_rotate_left_accumulator() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0b0000_0000);
        cpu.reg.set_flag(Flag::C, true);
        let (cpu, _bus, mnemonic) = test_instruction(&RotateLeftAccumulator, cpu, bus);

        assert_eq!(mnemonic, "RLA");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 4);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0b0000_0001);
        assert_eq!(cpu.reg.get_flag(Flag::C), false);
    }

    #[test]
    fn test_rotate_right_circular_accumulator() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0b0000_0001);
        let (cpu, _bus, mnemonic) = test_instruction(&RotateRightCircularAccumulator, cpu, bus);

        assert_eq!(mnemonic, "RRCA");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 4);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0b1000_0000);
        assert_eq!(cpu.reg.get_flag(Flag::C), true);
    }

    #[test]
    fn test_rotate_right_accumulator() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0b0000_0000);
        cpu.reg.set_flag(Flag::C, true);
        let (cpu, _bus, mnemonic) = test_instruction(&RotateRightAccumulator, cpu, bus);

        assert_eq!(mnemonic, "RRA");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 4);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0b1000_0000);
        assert_eq!(cpu.reg.get_flag(Flag::C), false);
    }

    #[test]
    fn test_load_8bit() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0x00);
        cpu.reg.set_8bit(Reg8::B, 0xff);
        let inst = &Load8Bit {
            dst: Reg8::A,
            src: Reg8::B,
        };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD A,B");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 4);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0xff);
    }

    #[test]
    fn test_load_8bit_immediate() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0x00);
        let inst = &Load8BitImmediate { reg: Reg8::A };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD A,$aa");
        assert_eq!(cpu.reg.get_pc(), 2);
        assert_eq!(cpu.mtime, 8);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0xaa);
    }

    #[test]
    fn test_load_8bit_indirect_to_bus() {
        let (mut cpu, mut bus) = fixtures();
        let addr = 0xc000;
        cpu.reg.set_16bit(Reg16::BC, addr);
        cpu.reg.set_8bit(Reg8::A, 0xff);
        bus.write_8bit(addr, 0x00);
        let inst = &Load8BitIndirect {
            direction: Direction::ToBus,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };
        let (cpu, bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD (BC),A");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 8);
        assert_eq!(bus.read_8bit(addr), 0xff);
    }

    #[test]
    fn test_load_8bit_indirect_to_register() {
        let (mut cpu, mut bus) = fixtures();
        let addr = 0xc000;
        cpu.reg.set_16bit(Reg16::BC, addr);
        cpu.reg.set_8bit(Reg8::A, 0x00);
        bus.write_8bit(addr, 0xff);
        let inst = &Load8BitIndirect {
            direction: Direction::ToRegister,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD A,(BC)");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 8);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0xff);
    }

    #[test]
    fn test_load_8bit_indirect_increment_to_bus() {
        let (mut cpu, mut bus) = fixtures();
        let addr = 0xc000;
        cpu.reg.set_16bit(Reg16::BC, addr);
        cpu.reg.set_8bit(Reg8::A, 0xff);
        bus.write_8bit(addr, 0x00);
        let inst = &Load8BitIndirectIncrement {
            direction: Direction::ToBus,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };
        let (cpu, bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD (BC+),A");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 8);
        assert_eq!(bus.read_8bit(addr), 0xff);
        assert_eq!(cpu.reg.get_16bit(Reg16::BC), addr.wrapping_add(1));
    }

    #[test]
    fn test_load_8bit_indirect_increment_to_register() {
        let (mut cpu, mut bus) = fixtures();
        let addr = 0xc000;
        cpu.reg.set_16bit(Reg16::BC, addr);
        cpu.reg.set_8bit(Reg8::A, 0x00);
        bus.write_8bit(addr, 0xff);
        let inst = &Load8BitIndirectIncrement {
            direction: Direction::ToRegister,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD A,(BC+)");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 8);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0xff);
        assert_eq!(cpu.reg.get_16bit(Reg16::BC), addr.wrapping_add(1));
    }

    #[test]
    fn test_load_8bit_indirect_decrement_to_bus() {
        let (mut cpu, mut bus) = fixtures();
        let addr = 0xc000;
        cpu.reg.set_16bit(Reg16::BC, addr);
        cpu.reg.set_8bit(Reg8::A, 0xff);
        bus.write_8bit(addr, 0x00);
        let inst = &Load8BitIndirectDecrement {
            direction: Direction::ToBus,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };
        let (cpu, bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD (BC-),A");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 8);
        assert_eq!(bus.read_8bit(addr), 0xff);
        assert_eq!(cpu.reg.get_16bit(Reg16::BC), addr.wrapping_sub(1));
    }

    #[test]
    fn test_load_8bit_indirect_decrement_to_register() {
        let (mut cpu, mut bus) = fixtures();
        let addr = 0xc000;
        cpu.reg.set_16bit(Reg16::BC, addr);
        cpu.reg.set_8bit(Reg8::A, 0x00);
        bus.write_8bit(addr, 0xff);
        let inst = &Load8BitIndirectDecrement {
            direction: Direction::ToRegister,
            addr_reg: Reg16::BC,
            reg: Reg8::A,
        };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD A,(BC-)");
        assert_eq!(cpu.reg.get_pc(), 1);
        assert_eq!(cpu.mtime, 8);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0xff);
        assert_eq!(cpu.reg.get_16bit(Reg16::BC), addr.wrapping_sub(1));
    }

    #[test]
    fn test_load_high_to_bus() {
        let (mut cpu, mut bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0xff);
        bus.write_8bit(0xffaa, 0x00);
        let inst = &LoadHigh {
            direction: Direction::ToBus,
        };
        let (cpu, bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LDH ($aa),A");
        assert_eq!(cpu.reg.get_pc(), 2);
        assert_eq!(cpu.mtime, 12);
        assert_eq!(bus.read_16bit(0xffaa), 0xff);
    }

    #[test]
    fn test_load_high_to_register() {
        let (mut cpu, mut bus) = fixtures();
        cpu.reg.set_8bit(Reg8::A, 0x00);
        bus.write_8bit(0xffaa, 0xff);
        let inst = &LoadHigh {
            direction: Direction::ToRegister,
        };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LDH A,($aa)");
        assert_eq!(cpu.reg.get_pc(), 2);
        assert_eq!(cpu.mtime, 12);
        assert_eq!(cpu.reg.get_8bit(Reg8::A), 0xff);
    }

    #[test]
    fn test_load_16bit_immediate() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_16bit(Reg16::BC, 0x0000);
        let inst = &Load16BitImmediate { reg: Reg16::BC };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD BC,$c6aa");
        assert_eq!(cpu.reg.get_pc(), 3);
        assert_eq!(cpu.mtime, 12);
        assert_eq!(cpu.reg.get_16bit(Reg16::BC), 0xc6aa);
    }

    #[test]
    fn test_load_16bit_indirect_immediate() {
        let (mut cpu, bus) = fixtures();
        cpu.reg.set_16bit(Reg16::SP, 0xffff);
        let inst = &Load16BitIndirectImmediate;
        let (cpu, bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "LD ($c6aa),SP");
        assert_eq!(cpu.reg.get_pc(), 3);
        assert_eq!(cpu.mtime, 20);
        assert_eq!(bus.read_16bit(0xc6aa), 0xffff);
    }

    #[test]
    fn test_jump_immediate_no_condition() {
        let (cpu, bus) = fixtures();
        let inst = &JumpImmediate { condition: None };
        let (cpu, _bus, mnemonic) = test_instruction(inst, cpu, bus);

        assert_eq!(mnemonic, "JP $c6aa");
        assert_eq!(cpu.reg.get_pc(), 0xc6aa);
        assert_eq!(cpu.mtime, 16);
    }
}
