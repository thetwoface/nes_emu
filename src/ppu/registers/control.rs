bitflags! {
    // 7  bit  0
    // ---- ----
    // VPHB SINN
    // |||| ||||
    // |||| ||++- Base nametable address
    // |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    // |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    // |||| |     (0: add 1, going across; 1: add 32, going down)
    // |||| +---- Sprite pattern table address for 8x8 sprites
    // ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    // |||+------ Background pattern table address (0: $0000; 1: $1000)
    // ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
    // |+-------- PPU master/slave select
    // |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    // +--------- Generate an NMI at the start of the
    //            vertical blanking interval (0: off; 1: on)
    pub struct ControlRegister: u8 {
        const NAMETABLE1             = 0b0000_0001;
        const NAMETABLE2             = 0b0000_0010;
        const VRAM_ADD_INCREMENT     = 0b0000_0100;
        const SPRITE_PATTERN_ADDR    = 0b0000_1000;
        const BACKROUND_PATTERN_ADDR = 0b0001_0000;
        const SPRITE_SIZE            = 0b0010_0000;
        const MASTER_SLAVE_SELECT    = 0b0100_0000;
        const GENERATE_NMI           = 0b1000_0000;
    }
}

impl ControlRegister {
    #[must_use]
    pub fn new() -> Self {
        Self::from_bits_truncate(0b0000_0000)
    }

    /// # Panics
    /// Will panic if something went wrong with nametable adsress bits
    #[must_use]
    pub fn nametable_addr(&self) -> u16 {
        match self.bits() & 0b11 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2c00,
            _ => panic!("not possible"),
        }
    }

    #[must_use]
    pub fn vram_addr_increment(&self) -> u8 {
        if self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
            32
        } else {
            1
        }
    }

    #[must_use]
    pub fn sprt_pattern_addr(&self) -> u16 {
        if self.contains(ControlRegister::SPRITE_PATTERN_ADDR) {
            0x1000
        } else {
            0
        }
    }

    #[must_use]
    pub fn bknd_pattern_addr(&self) -> u16 {
        if self.contains(ControlRegister::BACKROUND_PATTERN_ADDR) {
            0x1000
        } else {
            0
        }
    }

    #[must_use]
    pub fn sprite_size(&self) -> u8 {
        if self.contains(ControlRegister::SPRITE_SIZE) {
            16
        } else {
            8
        }
    }

    #[must_use]
    pub fn master_slave_select(&self) -> u8 {
        u8::from(self.contains(ControlRegister::MASTER_SLAVE_SELECT))
    }

    #[must_use]
    pub fn generate_vblank_nmi(&self) -> bool {
        self.contains(ControlRegister::GENERATE_NMI)
    }

    pub fn update(&mut self, data: u8) {
        // hope this works. if not - just use from_bits directly
        self.0 .0 = data;
    }
}

impl Default for ControlRegister {
    fn default() -> Self {
        Self::new()
    }
}
