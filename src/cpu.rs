use crate::registers::{Flag, Reg16, Reg8, Registers};

pub trait Compute {
    fn get_reg8(&self, reg: Reg8) -> u8;
    fn set_reg8(&mut self, reg: Reg8, value: u8);

    fn get_reg16(&self, reg: Reg16) -> u16;
    fn set_reg16(&mut self, reg: Reg16, value: u16);

    fn get_flag(&self, flag: Flag) -> bool;
    fn set_flag(&mut self, flag: Flag, set: bool);

    fn get_cycle_count(&self) -> u64;
    fn increment_cycle_count(&mut self, amount: u64);

    fn get_ime(&self) -> bool;
    fn set_ime(&mut self, set: bool);

    fn get_stopped(&self) -> bool;
    fn set_stopped(&mut self, set: bool);

    fn get_reg16_offset(&self, reg: Reg16, offset: i8) -> u16 {
        let value = self.get_reg16(reg);
        if offset.is_positive() {
            value.wrapping_add(offset as u16)
        } else {
            // two's complement
            value.wrapping_sub((offset as u16 ^ 0xffff) + 1)
        }
    }

    fn set_reg16_offset(&mut self, reg: Reg16, offset: i8) {
        let new_value = self.get_reg16_offset(reg, offset);
        self.set_reg16(reg, new_value);
    }
}

pub struct Cpu {
    reg: Registers,
    ime: bool,
    stopped: bool,
    cycles: u64,
}

impl Cpu {
    pub fn initialized() -> Cpu {
        Cpu {
            reg: Registers::initialized(),
            ime: true,
            stopped: false,
            cycles: 0,
        }
    }

    pub fn reset() -> Cpu {
        Cpu {
            reg: Registers::reset(),
            ime: true,
            stopped: false,
            cycles: 0,
        }
    }
}

impl Compute for Cpu {
    fn get_reg8(&self, reg: Reg8) -> u8 {
        self.reg.get_8bit(reg)
    }

    fn set_reg8(&mut self, reg: Reg8, value: u8) {
        self.reg.set_8bit(reg, value);
    }

    fn get_reg16(&self, reg: Reg16) -> u16 {
        self.reg.get_16bit(reg)
    }

    fn set_reg16(&mut self, reg: Reg16, value: u16) {
        self.reg.set_16bit(reg, value);
    }

    fn get_flag(&self, flag: Flag) -> bool {
        self.reg.get_flag(flag)
    }

    fn set_flag(&mut self, flag: Flag, set: bool) {
        self.reg.set_flag(flag, set);
    }

    fn get_cycle_count(&self) -> u64 {
        self.cycles
    }

    fn increment_cycle_count(&mut self, amount: u64) {
        self.cycles = self.cycles.wrapping_add(amount);
    }

    fn get_ime(&self) -> bool {
        self.ime
    }

    fn set_ime(&mut self, set: bool) {
        self.ime = set;
    }

    fn get_stopped(&self) -> bool {
        self.stopped
    }

    fn set_stopped(&mut self, set: bool) {
        self.stopped = set;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_reg16_offset() {
        let mut cpu = Cpu::reset();
        cpu.set_reg16(Reg16::PC, 0x0100);
        let addr = cpu.get_reg16_offset(Reg16::PC, -0x10);
        assert_eq!(addr, 0x00f0);
    }
}
