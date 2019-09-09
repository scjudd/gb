mod bus;
mod cpu;
mod instructions;
mod opcodes;
mod registers;
mod spies;

use crate::bus::{Address, Bus};
use crate::cpu::{Compute, Cpu};
use crate::opcodes::OPCODES;
use crate::registers::Reg16;
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("Usage: {} path/to/rom", args[0]);
        process::exit(1);
    }

    let mut cpu = Cpu::initialized();
    let mut bus = Bus::load_rom(&args[1]);

    loop {
        next(&mut cpu, &mut bus);
    }
}

fn next(mut cpu: &mut Cpu, mut bus: &mut Bus) {
    let pc = cpu.get_reg16(Reg16::PC);
    let opcode = bus.read_8bit(pc);
    let inst = OPCODES[opcode as usize];
    inst.execute(cpu, bus);
    println!("{:04x}: {:02x} {}", pc, opcode, inst.mnemonic(pc, bus));

    if cpu.get_ime() {
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

fn handle_interrupt(addr: u16, cpu: &mut Cpu, bus: &mut Bus) {
    cpu.set_ime(false);
    cpu.set_reg16_offset(Reg16::SP, -2);
    bus.write_16bit(cpu.get_reg16(Reg16::SP), cpu.get_reg16(Reg16::PC));
    cpu.set_reg16(Reg16::PC, addr);

    // This costs 12 cycles on top of however many the actual interrupt handler takes
}
