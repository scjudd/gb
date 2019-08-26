use std::fs::File;
use std::io::Read;

/// An (incomplete) implementation of the GameBoy's address bus.
///
/// Exists in order to provide the GameBoy's CPU the unified interface it needs to access its
/// various memories and peripherals.
pub struct AddressBus {
    rom: Vec<u8>,
    hram: [u8; 0x7f],
}

impl AddressBus {
    // | Start | End   | Description
    // |-------|-------|------------
    // | 0000  | 3fff  | 16KB ROM bank 00
    // | 4000  | 7fff  | 16KB ROM bank 01~NN
    // | 8000  | 9fff  | 8KB Video RAM (VRAM)
    // | a000  | bfff  | 8KB External RAM
    // | c000  | cfff  | 4KB Work RAM (WRAM) bank 0
    // | d000  | dfff  | 4KB Work RAM (WRAM) bank 1~N
    // | e000  | fdff  | Mirror of c000~ddff (ECHO RAM)
    // | fe00  | fe9f  | Sprite attribute table (OAM)
    // | fea0  | feff  | Not usable
    // | ff00  | ff7f  | I/O Registers
    // | ff80  | fffe  | High RAM (HRAM)
    // | ffff  | ffff  | Interrupts Enable Register (IE)
    // http://gbdev.gg8.se/wiki/articles/Memory_Map

    /// Opens a ROM file and reads it into memory, returning an AddressBus
    pub fn load_rom(rom_path: &str) -> AddressBus {
        let mut f = File::open(rom_path).expect("could not open ROM");

        let mut rom = Vec::new();
        f.read_to_end(&mut rom)
            .expect("couldn't read ROM into Vec<u8>");

        let hram = [0; 0x7f];

        AddressBus { rom, hram }
    }

    pub fn read_8bit(&self, addr: u16) -> u8 {
        if addr <= 0x8000 {
            return self.rom[addr as usize];
        }

        if addr >= 0xff80 && addr <= 0xfffe {
            return self.hram[(addr - 0xff80) as usize];
        }

        panic!(
            "Attempted to read from unmapped memory address ${:04x}",
            addr
        );
    }

    pub fn read_16bit(&self, addr: u16) -> u16 {
        let low = self.read_8bit(addr);
        let high = self.read_8bit(addr.wrapping_add(1));
        ((high as u16) << 8) | (low as u16)
    }

    pub fn write_8bit(&mut self, addr: u16, value: u8) {
        if addr <= 0x8000 {
            // When we delegate to a Cart module that implements a given MBC, it can perform
            // whatever sort of ROM/RAM bank switching on writes into this address space. For now,
            // we'll just panic.
            panic!("Attempted to write to read-only memory, and ROM/RAM bank switching is not yet implemented.");
        }

        if addr >= 0xff80 && addr <= 0xfffe {
            self.hram[(addr - 0xff80) as usize] = value;
            return;
        }

        panic!(
            "Attempted to write to unmapped memory address ${:04x}",
            addr
        );
    }

    pub fn write_16bit(&mut self, addr: u16, value: u16) {
        self.write_8bit(addr, value as u8);
        self.write_8bit(addr.wrapping_add(1), (value >> 8) as u8);
    }
}
