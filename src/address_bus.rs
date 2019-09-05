use std::fs::File;
use std::io::Read;

/// An (incomplete) implementation of the GameBoy's address bus.
///
/// Exists in order to provide the GameBoy's CPU the unified interface it needs to access its
/// various memories and peripherals.
pub struct AddressBus {
    rom: Vec<u8>,
    vram: [u8; 0x2000],
    ext_ram: [u8; 0x2000],
    wram: [u8; 0x2000],
    io_registers: [u8; 0x80],
    hram: [u8; 0x7f],
    ie_register: u8,
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

        AddressBus {
            rom,
            vram: [0; 0x2000],
            ext_ram: [0; 0x2000],
            wram: [0; 0x2000],
            io_registers: [0; 0x80],
            hram: [0; 0x7f],
            ie_register: 0,
        }
    }

    pub fn from_raw(data: &[u8]) -> AddressBus {
        let mut rom: Vec<u8>;

        if data.len() < 0x8000 {
            let zeroes = 0x8000 - data.len();
            rom = Vec::with_capacity(0x8000);
            for byte in data {
                rom.push(*byte);
            }
            for _ in 0..zeroes {
                rom.push(0x00);
            }
        } else {
            rom = Vec::from(data);
        }

        AddressBus {
            rom,
            vram: [0; 0x2000],
            ext_ram: [0; 0x2000],
            wram: [0; 0x2000],
            io_registers: [0; 0x80],
            hram: [0; 0x7f],
            ie_register: 0,
        }
    }

    pub fn read_8bit(&self, addr: u16) -> u8 {
        if addr <= 0x8000 {
            return self.rom[addr as usize];
        }

        if addr >= 0x8000 && addr <= 0x9fff {
            return self.vram[(addr - 0x8000) as usize];
        }

        if addr >= 0xa000 && addr <= 0xbfff {
            return self.ext_ram[(addr - 0xa000) as usize];
        }

        if addr >= 0xc000 && addr <= 0xdfff {
            return self.wram[(addr - 0xc000) as usize];
        }

        if addr >= 0xff00 && addr <= 0xff7f {
            return self.io_registers[(addr - 0xff00) as usize];
        }

        if addr >= 0xff80 && addr <= 0xfffe {
            return self.hram[(addr - 0xff80) as usize];
        }

        if addr == 0xffff {
            return self.ie_register;
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

        if addr >= 0x8000 && addr <= 0x9fff {
            self.vram[(addr - 0x8000) as usize] = value;
            return;
        }

        if addr >= 0xa000 && addr <= 0xbfff {
            self.ext_ram[(addr - 0xa000) as usize] = value;
            return;
        }

        if addr >= 0xc000 && addr <= 0xdfff {
            self.wram[(addr - 0xc000) as usize] = value;
            return;
        }

        if addr >= 0xff00 && addr <= 0xff7f {
            self.io_registers[(addr - 0xff00) as usize] = value;
            return;
        }

        if addr >= 0xff80 && addr <= 0xfffe {
            self.hram[(addr - 0xff80) as usize] = value;
            return;
        }

        if addr == 0xffff {
            self.ie_register = value;
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
