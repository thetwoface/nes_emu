pub struct AddressRegister {
    value: (u8, u8),
    hi_ptr: bool,
}

impl AddressRegister {
    pub fn new() -> Self {
        Self {
            value: (0, 0), // // high byte first, low byte second
            hi_ptr: true,
        }
    }

    fn set(&mut self, data: u16) {
        self.value.0 = (data >> 8) as u8;
        self.value.1 = (data & 0xff) as u8;
    }

    pub fn get(&self) -> u16 {
        (u16::from(self.value.0) << 8) | u16::from(self.value.1)
    }

    pub fn update(&mut self, data: u8) {
        if self.hi_ptr {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }

        // mirror down addr above 0x3FFF
        if self.get() > 0x3FFF {
            self.set(self.get() & 0b11_1111_1111_1111);
        }

        self.hi_ptr = !self.hi_ptr;
    }

    pub fn increment(&mut self, inc: u8) {
        let lo = self.value.1;
        self.value.1 = self.value.1.wrapping_add(inc);

        if lo > self.value.1 {
            self.value.0 = self.value.0.wrapping_add(1);
        }

        //mirror down addr above 0x3FFF
        if self.get() > 0x3FFF {
            self.set(self.get() & 0b11111111111111);
        }
    }

    pub fn reset_latch(&mut self) {
        self.hi_ptr = true;
    }
}
