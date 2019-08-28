use crate::registers::Registers;

pub struct CPU {
    pub reg: Registers,
    pub ime: bool,
    pub stopped: bool,
}

impl CPU {
    pub fn initialized() -> CPU {
        CPU {
            reg: Registers::initialized(),
            ime: true,
            stopped: false,
        }
    }
}
