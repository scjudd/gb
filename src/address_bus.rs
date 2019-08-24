pub struct AddressBus {
    // Because this is a static array, we can't do any sort of ROM/RAM banking, so this is useless
    // for most games, but a good quick way to load up a real ROM and start running until it
    // crashes.
    rom: [u8; 0x4000],
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

    /// Opens a ROM file and reads the first 0x4000 bytes (ROM bank 00) into memory, returning an
    /// AddressBus
    ///
    /// This is not a useful interface for running an actual ROM dump unless the ROM does not use
    /// any sort of memory paging. It's a half-assed implementation, so that I can spend cycles on
    /// more interesting things.
    pub fn load_rom_bank_0(rom_path: &str) -> AddressBus {
        use std::fs::File;
        use std::io::Read;
        let mut f = File::open(rom_path).expect("could not open ROM");

        let mut rom = [0; 0x4000];
        f.read(&mut rom)
            .expect("couldn't read ROM bank 00 into array");

        let hram = [0; 0x7f];

        AddressBus { rom, hram }
    }

    pub fn read_8bit(&self, addr: u16) -> u8 {
        if addr <= 0x4000 {
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
        if addr <= 0x4000 {
            return;
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
