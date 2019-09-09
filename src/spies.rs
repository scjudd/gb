#![cfg(test)]

use crate::bus::{Address, FakeBus};
use crate::cpu::{Compute, Cpu};
use crate::registers::{Flag, Reg16, Reg8};
use std::collections::HashMap;

/// BusSpy enables us to wrap a Bus, enable recording mode, and capture all of the changes an
/// instruction makes to the wrapped Bus. This way, we can ensure that _only_ the expected changes
/// occur.
pub struct BusSpy {
    bus: FakeBus,
    recording: bool,
    expectations_8bit: HashMap<u16, u8>,
    changes_8bit: HashMap<u16, u8>,
    expectations_16bit: HashMap<u16, u16>,
    changes_16bit: HashMap<u16, u16>,
}

impl BusSpy {
    pub fn wrap(bus: FakeBus) -> BusSpy {
        BusSpy {
            bus: bus,
            recording: false,
            expectations_8bit: HashMap::new(),
            changes_8bit: HashMap::new(),
            expectations_16bit: HashMap::new(),
            changes_16bit: HashMap::new(),
        }
    }

    pub fn record_changes(&mut self) {
        self.recording = true;
    }

    pub fn expect_8bit(&mut self, addr: u16, value: u8) {
        self.expectations_8bit.insert(addr, value);
    }

    pub fn expect_16bit(&mut self, addr: u16, value: u16) {
        self.expectations_16bit.insert(addr, value);
    }

    pub fn assert_expectations(&self) {
        assert_eq!(self.expectations_8bit, self.changes_8bit);
        assert_eq!(self.expectations_16bit, self.changes_16bit);
    }
}

impl Address for BusSpy {
    fn read_8bit(&self, addr: u16) -> u8 {
        self.bus.read_8bit(addr)
    }

    fn write_8bit(&mut self, addr: u16, value: u8) {
        if self.recording {
            self.changes_8bit.insert(addr, value);
        }

        self.bus.write_8bit(addr, value);
    }

    fn read_16bit(&self, addr: u16) -> u16 {
        self.bus.read_16bit(addr)
    }

    fn write_16bit(&mut self, addr: u16, value: u16) {
        if self.recording {
            self.changes_16bit.insert(addr, value);
        }

        self.bus.write_16bit(addr, value);
    }
}

/// CpuSpy enables us to wrap a Cpu, enable recording mode, and capture all of the changes an
/// instruction makes to the wrapped Cpu. This way, we can ensure that _only_ the expected changes
/// occur.
pub struct CpuSpy {
    cpu: Cpu,
    recording: bool,
    reg8_expectations: HashMap<Reg8, u8>,
    reg8_changes: HashMap<Reg8, u8>,
    reg16_expectations: HashMap<Reg16, u16>,
    reg16_changes: HashMap<Reg16, u16>,
    flag_expectations: HashMap<Flag, bool>,
    flag_changes: HashMap<Flag, bool>,
    expected_cycles: Option<u64>,
    cycles_changed: bool,
    expected_stop_change: Option<bool>,
    stop_changed: bool,
    expected_ime_change: Option<bool>,
    ime_changed: bool,
}

impl CpuSpy {
    pub fn wrap(cpu: Cpu) -> CpuSpy {
        CpuSpy {
            cpu: cpu,
            recording: false,
            reg8_expectations: HashMap::new(),
            reg8_changes: HashMap::new(),
            reg16_expectations: HashMap::new(),
            reg16_changes: HashMap::new(),
            flag_expectations: HashMap::new(),
            flag_changes: HashMap::new(),
            expected_cycles: None,
            cycles_changed: false,
            expected_stop_change: None,
            stop_changed: false,
            expected_ime_change: None,
            ime_changed: false,
        }
    }

    pub fn record_changes(&mut self) {
        self.recording = true;
    }

    pub fn expect_reg8(&mut self, reg: Reg8, value: u8) {
        self.reg8_expectations.insert(reg, value);
    }

    pub fn expect_reg16(&mut self, reg: Reg16, value: u16) {
        self.reg16_expectations.insert(reg, value);
    }

    pub fn expect_flag(&mut self, flag: Flag, set: bool) {
        self.flag_expectations.insert(flag, set);
    }

    pub fn expect_cycles(&mut self, cycles: u64) {
        self.expected_cycles = Some(cycles);
    }

    pub fn expect_stop(&mut self, set: bool) {
        self.expected_stop_change = Some(set);
    }

    pub fn expect_ime(&mut self, set: bool) {
        self.expected_ime_change = Some(set);
    }

    pub fn assert_expectations(&self) {
        assert_eq!(self.reg8_expectations, self.reg8_changes);
        assert_eq!(self.reg16_expectations, self.reg16_changes);
        assert_eq!(self.flag_expectations, self.flag_changes);

        if self.cycles_changed {
            match self.expected_cycles {
                None => panic!("unexpected CPU cycle count change"),
                Some(expected) => assert_eq!(self.cpu.get_cycle_count(), expected),
            }
        }

        if self.stop_changed {
            match self.expected_stop_change {
                None => panic!("unexpected CPU stop flag change"),
                Some(expected) => assert_eq!(self.cpu.get_stopped(), expected),
            }
        }

        if self.ime_changed {
            match self.expected_ime_change {
                None => panic!("unexpected CPU interrupt master enable flag change"),
                Some(expected) => assert_eq!(self.cpu.get_ime(), expected),
            }
        }
    }
}

impl Compute for CpuSpy {
    fn get_reg8(&self, reg: Reg8) -> u8 {
        self.cpu.get_reg8(reg)
    }

    fn set_reg8(&mut self, reg: Reg8, value: u8) {
        if self.recording {
            self.reg8_changes.insert(reg, value);
        }

        self.cpu.set_reg8(reg, value);
    }

    fn get_reg16(&self, reg: Reg16) -> u16 {
        self.cpu.get_reg16(reg)
    }

    fn set_reg16(&mut self, reg: Reg16, value: u16) {
        if self.recording {
            self.reg16_changes.insert(reg, value);
        }

        self.cpu.set_reg16(reg, value);
    }

    fn get_flag(&self, flag: Flag) -> bool {
        self.cpu.get_flag(flag)
    }

    fn set_flag(&mut self, flag: Flag, set: bool) {
        if self.recording {
            self.flag_changes.insert(flag, set);
        }

        self.cpu.set_flag(flag, set);
    }

    fn get_cycle_count(&self) -> u64 {
        self.cpu.get_cycle_count()
    }

    fn increment_cycle_count(&mut self, amount: u64) {
        if self.recording {
            self.cycles_changed = true;
        }

        self.cpu.increment_cycle_count(amount);
    }

    fn get_ime(&self) -> bool {
        self.cpu.get_ime()
    }

    fn set_ime(&mut self, set: bool) {
        if self.recording {
            self.ime_changed = true;
        }

        self.cpu.set_ime(set);
    }

    fn get_stopped(&self) -> bool {
        self.cpu.get_stopped()
    }

    fn set_stopped(&mut self, set: bool) {
        if self.recording {
            self.stop_changed = true;
        }

        self.cpu.set_stopped(set);
    }
}
