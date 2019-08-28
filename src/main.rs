mod address_bus;
mod cpu;
mod instructions;
mod opcodes;
mod registers;

use crate::address_bus::AddressBus;
use crate::cpu::CPU;
use crate::opcodes::OPCODES;
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} path/to/rom", args[0]);
        process::exit(1);
    }

    let mut cpu = CPU::initialized();
    let mut bus = AddressBus::load_rom(&args[1]);

    loop {
        next(&mut cpu, &mut bus);
    }
}

fn next(mut cpu: &mut CPU, mut bus: &mut AddressBus) {
    let pc = cpu.reg.get_pc();
    let opcode = bus.read_8bit(pc);
    let inst = OPCODES[opcode as usize];
    inst.execute(cpu, bus);
    println!("{:04x}: {:02x} {}", pc, opcode, inst.mnemonic(pc, bus));

    if cpu.ime {
        let reg_if = bus.read_8bit(0xff0f);
        let reg_ie = bus.read_8bit(0xffff);
        let fired = reg_if & reg_ie;

        // Vblank
        if fired & 0x01 == 0x01 {
            bus.write_8bit(0xff0f, reg_if & !0x01); // Don't fire again
            handle_interrupt(0x40, &mut cpu, &mut bus);
        }
    }

    // TODO: use num_cycles and measure our current execution speed to throttle our execution speed
    // to 4.20MHz for DMG, 8.4MHz for CGB in double-speed mode.
}

fn handle_interrupt(addr: u16, cpu: &mut CPU, bus: &mut AddressBus) {
    cpu.ime = false;
    cpu.reg.set_sp_offset(-2);
    bus.write_16bit(cpu.reg.get_sp(), cpu.reg.get_pc());
    cpu.reg.set_pc(addr);

    // This costs 12 cycles on top of however many the actual interrupt handler takes
}
