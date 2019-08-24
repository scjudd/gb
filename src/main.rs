mod address_bus;
mod exec_state;
mod instructions;
mod opcodes;
mod registers;

use crate::address_bus::AddressBus;
use crate::exec_state::ExecState;
use crate::opcodes::OPCODES;
use crate::registers::Registers;
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} path/to/rom", args[0]);
        process::exit(1);
    }

    let mut state = ExecState::new();
    let mut reg = Registers::new();
    let mut bus = AddressBus::load_rom_bank_0(&args[1]);

    reg.set_pc(0x0100);

    for _ in 0..0x10 {
        next(&mut reg, &mut bus, &mut state);
    }
}

fn next(reg: &mut Registers, bus: &mut AddressBus, state: &mut ExecState) {
    let pc = reg.get_pc();
    let opcode = bus.read_8bit(pc);
    let inst = OPCODES[opcode as usize];
    inst.execute(reg, bus, state);
    println!("{:04x}: {:02x} {}", pc, opcode, inst.mnemonic(pc, bus));

    // TODO: use num_cycles and measure our current execution speed to throttle our execution speed
    // to 4.20MHz for DMG, 8.4MHz for CGB in double-speed mode.
}
