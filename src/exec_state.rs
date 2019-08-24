// Initial values:
// * IME = 0
// * IF  = 0xe0
// * IE  = 0x00
// https://www.reddit.com/r/EmuDev/comments/7zx1om/gb_value_of_interrupt_master_enable_ime_at_startup/duv0085

pub struct ExecState {
    pub interrupts_enabled: bool,
}

impl ExecState {
    pub fn new() -> ExecState {
        ExecState {
            interrupts_enabled: false,
        }
    }
}
