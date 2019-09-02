use crate::registers::Registers;

pub struct CPU {
    pub mtime: usize,
    pub reg: Registers,
    pub ime: bool,
    pub stopped: bool,
}

impl CPU {
    pub fn initialized() -> CPU {
        CPU {
            mtime: 0,
            reg: Registers::initialized(),
            ime: true,
            stopped: false,
        }
    }

    pub fn inc_mtime(&mut self, amount: usize) {
        self.mtime = self.mtime.wrapping_add(amount);
    }
}
