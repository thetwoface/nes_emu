use std::collections::HashMap;

use crate::opcodes;

bitflags! {
    /// # Status Register (P) http://wiki.nesdev.com/w/index.php/Status_flags
    ///
    ///  7 6 5 4 3 2 1 0
    ///  N V _ B D I Z C
    ///  | |   | | | | +--- Carry Flag
    ///  | |   | | | +----- Zero Flag
    ///  | |   | | +------- Interrupt Disable
    ///  | |   | +--------- Decimal Mode (not used on NES)
    ///  | |   +----------- Break Command
    ///  | +--------------- Overflow Flag
    ///  +----------------- Negative Flag
    ///
    pub struct CpuFlags:u8{
        const CARRY             = 0b0000_0001;
        const ZERO              = 0b0000_0010;
        const INTERRUPT_DISABLE = 0b0000_0100;
        const DECIMAL_MODE      = 0b0000_1000;
        const BREAK             = 0b0001_0000;
        const BREAK2            = 0b0010_0000;
        const OVERFLOW          = 0b0100_0000;
        const NEGATIVE          = 0b1000_0000;
    }
}

#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    NoneAddressing,
}

#[must_use]
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub memory: [u8; 0xFFFF],
}

trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&self, pos: u16) -> u16 {
        // consider https://doc.rust-lang.org/std/primitive.u16.html#method.from_le_bytes
        let lo = u16::from(self.mem_read(pos));
        let hi = u16::from(self.mem_read(pos + 1));
        (hi << 8) | (lo)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        // consider https://doc.rust-lang.org/std/primitive.u16.html#method.from_le_bytes
        let hi = (data >> 8) as u8;
        let lo = (data & 0xFF) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b10_0100),
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = CpuFlags::from_bits_truncate(0b10_0100);

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: &[u8]) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(program);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    /// # Panics
    ///
    /// Will panic if opcode is not recognized
    pub fn run(&mut self) {
        let opcodes: &HashMap<u8, &'static opcodes::OpCode> = &opcodes::OPCODES_MAP;

        loop {
            // Fetch next execution instruction from the instruction memory
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;

            let program_counter_state = self.program_counter;
            let opcode = opcodes
                .get(&code)
                .unwrap_or_else(|| panic!("OpCode {code:x} is not recognized"));

            // Decode the instruction
            match code {
                // LDA - Load Accumulator
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }

                // STA - Store Accumulator
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }

                // ADC - Add with Carry
                0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 71 => {
                    self.adc(&opcode.mode);
                }

                // SBC
                0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => {
                    self.sbc(&opcode.mode);
                }

                // AND - Logical AND
                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
                    self.and(&opcode.mode);
                }

                // ASL - Arithmetic Shift Left
                0x0A => self.asl_accumulator(),
                0x06 | 0x16 | 0x0E | 0x1E => self.asl(&opcode.mode),

                0xAA => self.tax(),

                0xE8 => self.inx(),

                // BRK - Force Interrupt
                // https://www.nesdev.org/obelisk-6502-guide/reference.html#BRK
                0x00 => return,

                _ => todo!(""),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += u16::from(opcode.len - 1);
            }
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::ZeroPage => u16::from(self.mem_read(self.program_counter)),

            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::ZeroPageX => {
                let pos = self.mem_read(self.program_counter);
                u16::from(pos.wrapping_add(self.register_x))
            }

            AddressingMode::ZeroPageY => {
                let pos = self.mem_read(self.program_counter);
                u16::from(pos.wrapping_add(self.register_y))
            }

            AddressingMode::AbsoluteX => {
                let base = self.mem_read_u16(self.program_counter);
                base.wrapping_add(u16::from(self.register_x))
            }

            AddressingMode::AbsoluteY => {
                let base = self.mem_read_u16(self.program_counter);
                base.wrapping_add(u16::from(self.register_y))
            }

            AddressingMode::IndirectX => {
                let base = self.mem_read(self.program_counter);
                let ptr = base.wrapping_add(self.register_x);
                let lo = self.mem_read(u16::from(ptr));
                let hi = self.mem_read(u16::from(ptr.wrapping_add(1)));
                (u16::from(hi)) << 8 | (u16::from(lo))
            }

            AddressingMode::IndirectY => {
                let base = self.mem_read(self.program_counter);
                let lo = self.mem_read(u16::from(base));
                let hi = self.mem_read(u16::from(base.wrapping_add(1)));
                let deref_base = (u16::from(hi)) << 8 | (u16::from(lo));
                deref_base.wrapping_add(u16::from(self.register_y))
            }

            AddressingMode::NoneAddressing => {
                panic!("mode {mode:?} is not supported");
            }
        }
    }

    /**
     * Set Zero flag if result = 0
     * Set Negative Flag if bit 7 of result is set
     */
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        if result & 0b1000_0000 == 0 {
            self.status.remove(CpuFlags::NEGATIVE);
        } else {
            self.status.insert(CpuFlags::NEGATIVE);
        }
    }

    /**
     * LDA - Load Accumulator
     * Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA>
     */
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /**
     * TAX - Transfer Accumulator to X
     * Copies the current contents of the accumulator into the X register and sets the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#TAX>
     */
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /**
     * INX - Increment X Register
     * Adds one to the X register setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#INX>
     */
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /**
     * STA - Store Accumulator
     * Stores the contents of the accumulator into memory.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#STA>
     */
    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    /**
     * ADC - Add with Carry
     * This instruction adds the contents of a memory location to the accumulator together with the carry bit.
     * If overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#ADC>
     */
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.add_to_register_a(value);
    }

    /**
     * SBC - Subtract with Carry
     * This instruction subtracts the contents of a memory location to the accumulator together with the not of the carry bit.
     * If overflow occurs the carry bit is clear, this enables multiple byte subtraction to be performed.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#SBC>
     */
    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        // A - B = A + (-B)
        // -B = !B + 1
        #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    /**
     * AND - Logical AND
     * A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#AND>
     */
    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.set_register_a(self.register_a & data);
    }

    /**
     * ASL - Arithmetic Shift Left
     * A,Z,C,N = M*2 or M,Z,C,N = M*2
     * This operation shifts all the bits of the accumulator or memory contents one bit left. Bit 0 is set to 0 and bit 7 is placed in the carry flag.
     * The effect of this operation is to multiply the memory contents by 2 (ignoring 2's complement considerations),
     * setting the carry if the result will not fit in 8 bits.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#ASL>
     */
    fn asl(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data <<= 1;
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn asl_accumulator(&mut self) {
        let mut data = self.register_a;

        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data <<= 1;
        self.set_register_a(data);
    }

    fn add_to_register_a(&mut self, data: u8) {
        // sum accumulator, data and carry flag if set
        let sum = u16::from(self.register_a)
            + u16::from(data)
            + u16::from(self.status.contains(CpuFlags::CARRY));

        // check if we overflow
        let carry = sum > 0xff;

        if carry {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        #[allow(clippy::cast_possible_truncation)]
        let result = sum as u8;

        if (data ^ result) & (result ^ self.register_a) & 0x80 == 0 {
            self.status.remove(CpuFlags::OVERFLOW);
        } else {
            self.status.insert(CpuFlags::OVERFLOW);
        }

        self.set_register_a(result);
    }

    fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn set_carry_flag(&mut self) {
        self.status.insert(CpuFlags::CARRY);
    }
    fn clear_carry_flag(&mut self) {
        self.status.remove(CpuFlags::CARRY);
    }
}

impl Default for CPU {
    fn default() -> Self {
        CPU::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b00);
        assert!(cpu.status.bits() & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xa9, 0xff, 0x00]);
        assert!(cpu.status.bits() & 0b1000_0000 == 0b1000_0000);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(&vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xa9, 0x0A, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_add_with_carry() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x01, 0x69, 0x01]);

        assert_eq!(cpu.register_a, 0x02);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
    }

    #[test]
    fn test_add_with_carry_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0x69, 0xFF]);

        assert_eq!(cpu.register_a, 0xFE);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
    }

    #[test]
    fn test_add_with_carry_overflow_and_zero() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0x69, 0x01]);

        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
    }

    #[test]
    fn test_add_with_carry_carries() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0x69, 0x01, 0x69, 0x01]);

        assert_eq!(cpu.register_a, 0x02);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
    }

    #[test]
    fn test_subtract_with_carry() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x02, 0xE9, 0x01]);

        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
    }

    #[test]
    fn test_subtract_with_carry_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x00, 0xE9, 0x01]);

        assert_eq!(cpu.register_a, 0xFE);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
    }

    #[test]
    fn test_logical_and() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0x29, 0xF0]);

        assert_eq!(cpu.register_a, 0xF0);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
    }

    #[test]
    fn test_arithmetic_shift_left_1() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x80, 0x0A]);

        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
    }

    #[test]
    fn test_arithmetic_shift_left_2() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x08, 0x0A]);

        assert_eq!(cpu.register_a, 0x10);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
    }
}
