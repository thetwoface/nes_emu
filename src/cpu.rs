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
    #[repr(transparent)]
    #[derive(Clone)]
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

const STACK_ADDR: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

#[must_use]
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub memory: [u8; 0xFFFF + 1], // from 0x0000 to 0xFFFF
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
            stack_pointer: STACK_RESET,
            program_counter: 0,
            memory: [0; 0xFFFF + 1],
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
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
                0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => self.lda(&opcode.mode),

                // LDX - Load X Register
                0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => self.ldx(&opcode.mode),

                // LDY - Load Y Register
                0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => self.ldy(&opcode.mode),

                // STA - Store Accumulator
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => self.sta(&opcode.mode),

                // STX - Store X Register
                0x86 | 0x96 | 0x8E => self.stx(&opcode.mode),

                // STY - Store Y Register
                0x84 | 0x94 | 0x8C => self.sty(&opcode.mode),

                // ADC - Add with Carry
                0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 71 => self.adc(&opcode.mode),

                // SBC
                0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => self.sbc(&opcode.mode),

                // AND - Logical AND
                0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => self.and(&opcode.mode),

                // EOR - Exclusive OR
                0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => self.eor(&opcode.mode),

                // ORA - Logical Inclusive OR
                0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => self.ora(&opcode.mode),

                // ASL - Arithmetic Shift Left
                0x0A => self.asl_accumulator(),
                0x06 | 0x16 | 0x0E | 0x1E => self.asl(&opcode.mode),

                // LSR - Logical Shift Right
                0x4A => self.lsr_accumulator(),
                0x46 | 0x56 | 0x4E | 0x5E => self.lsr(&opcode.mode),

                // ROL - Rotate Left
                0x2A => self.rol_accumulator(),
                0x26 | 0x36 | 0x2E | 0x3E => self.rol(&opcode.mode),

                // ROR - Rotate Right
                0x6A => self.ror_accumulator(),
                0x66 | 0x76 | 0x6E | 0x7E => self.ror(&opcode.mode),

                // BCC - Branch if Carry Clear
                0x90 => self.bcc(),

                // BCS - Branch if Carry Set
                0xB0 => self.bcs(),

                // BEQ - Branch if Equal
                0xF0 => self.beq(),

                // BNE - Branch if Not Equal
                0xD0 => self.bne(),

                // BMI - Branch if Minus
                0x30 => self.bmi(),

                // BPL - Branch if Positive
                0x10 => self.bpl(),

                // BVC - Branch if Overflow Clear
                0x50 => self.bvc(),

                // BVS - Branch if Overflow Set
                0x70 => self.bvs(),

                // BIT - Bit Test
                0x24 | 0x2C => self.bit(&opcode.mode),

                // CLC - Clear Carry Flag
                0x18 => self.clc(),

                // SEC - Set Carry Flag
                0x38 => self.sec(),

                // CLD - Clear Decimal Mode
                0xD8 => self.cld(),

                // SED - Set Decimal Flag
                0xF8 => self.sed(),

                // CLI - Clear Interrupt Disable
                0x58 => self.cli(),

                // SEI - Set Interrupt Disable
                0x78 => self.sei(),

                // CLV - Clear Overflow Flag
                0xB8 => self.clv(),

                // CMP - Compare
                0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => self.cmp(&opcode.mode),

                // CPX - Compare X Register
                0xE0 | 0xE4 | 0xEC => self.cpx(&opcode.mode),

                // CPY - Compare Y Register
                0xC0 | 0xC4 | 0xCC => self.cpy(&opcode.mode),

                // DEC - Decrement Memory
                0xC6 | 0xD6 | 0xCE | 0xDE => self.dec(&opcode.mode),

                // DEX - Decrement X Register
                0xCA => self.dex(),

                // DEY - Decrement Y Register
                0x88 => self.dey(),

                // TAX - Transfer Accumulator to X
                0xAA => self.tax(),

                // TAY - Transfer Accumulator to Y
                0xA8 => self.tay(),

                // TSX - Transfer Stack Pointer to X
                0xBA => self.tsx(),

                // TXA - Transfer X to Accumulator
                0x8A => self.txa(),

                // TYA - Transfer Y to Accumulator
                0x98 => self.tya(),

                // TXS - Transfer X to Stack Pointer
                0x9A => self.txs(),

                // INX - Increment X Register
                0xE8 => self.inx(),

                // INY - Increment Y Register
                0xC8 => self.iny(),

                // INC - Increment Memory
                0xE6 | 0xF6 | 0xEE | 0xFE => self.inc(&opcode.mode),

                // JMP - Jump Absolute
                0x4C => self.jmp_absolute(),
                // JMP - Jump Indirect
                0x6C => self.jmp_indirect(),

                // JSR - Jump to Subroutine
                0x20 => self.jsr(&opcode.mode),

                // RTS - Return from Subroutine
                0x60 => self.rts(),

                // PHA - Push Accumulator
                0x48 => self.pha(),

                // PLA - Pull Accumulator
                0x68 => self.pla(),

                // PHP - Push Processor Status
                0x08 => self.php(),

                // PLP - Pull Processor Status
                0x28 => self.plp(),

                // RTI - Return from Interrupt
                0x40 => self.rti(),

                // BRK - Force Interrupt
                // https://www.nesdev.org/obelisk-6502-guide/reference.html#BRK
                0x00 => {
                    self.brk();
                    return;
                }

                // NOP - No Operation
                0xEA => {
                    // The NOP instruction causes no changes to the processor
                    // other than the normal incrementing of the program counter to the next instruction.
                }

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

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(STACK_ADDR + u16::from(self.stack_pointer))
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write(STACK_ADDR + u16::from(self.stack_pointer), data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = u16::from(self.stack_pop());
        let hi = u16::from(self.stack_pop());

        hi << 8 | lo
    }

    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;

        self.stack_push(hi);
        self.stack_push(lo);
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
     * BRK - Force Interrupt
     * The BRK instruction forces the generation of an interrupt request.
     * The program counter and processor status are pushed on the stack then the IRQ interrupt vector at $FFFE/F is loaded into the PC
     * and the break flag in the status set to one.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BRK>
     */
    fn brk(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.stack_push(self.status.bits());
        self.program_counter = self.mem_read_u16(0xFFFE);
        self.status.insert(CpuFlags::BREAK);
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
     * LDX - Load X Register
     * Loads a byte of memory into the X register setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#LDX>
     */
    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /**
     * LDY - Load Y Register
     * Loads a byte of memory into the Y register setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#LDY>
     */
    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
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
     * TAY - Transfer Accumulator to Y
     * Copies the current contents of the accumulator into the Y register and sets the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#TAY>
     */
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    /**
     * TSX - Transfer Stack Pointer to X
     * Copies the current contents of the stack register into the X register and sets the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#TSX>
     */
    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /**
     * TXA - Transfer X to Accumulator
     * Copies the current contents of the X register into the accumulator and sets the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#TXA>
     */
    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /**
     * TYA - Transfer Y to Accumulator
     * Copies the current contents of the Y register into the accumulator and sets the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#TYA>
     */
    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /**
     * TXS - Transfer X to Stack Pointer
     * Copies the current contents of the X register into the stack register.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#TXS>
     */
    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
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
     * INY - Increment Y Register
     * Adds one to the Y register setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#INY>
     */
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    /**
     * INC - Increment Memory
     * Adds one to the value held at a specified memory location setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#INC>
     */
    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let result = data.wrapping_add(1);

        self.update_zero_and_negative_flags(result);

        self.mem_write(addr, result);
    }

    /**
     * DEC - Decrement Memory
     * Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#DEC>
     */
    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let result = data.wrapping_sub(1);

        self.update_zero_and_negative_flags(result);

        self.mem_write(addr, result);
    }

    /**
     * DEX - Decrement X Register
     * Subtracts one from the X register setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#DEX>
     */
    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /**
     * DEY - Decrement Y Register
     * Subtracts one from the Y register setting the zero and negative flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#DEY>
     */
    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
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
     * STX - Store X Register
     * Stores the contents of the X register into memory.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#STX>
     */
    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    /**
     * STY - Store Y Register
     * Stores the contents of the Y register into memory.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#STY>
     */
    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
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
     * EOR - Exclusive OR
     * An exclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#EOR>
     */
    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.set_register_a(self.register_a ^ data);
    }

    /**
     * ORA - Logical Inclusive OR
     * An inclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#ORA>
     */
    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        self.set_register_a(self.register_a | data);
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

    /**
     * LSR - Logical Shift Right
     * A,C,Z,N = A/2 or M,C,Z,N = M/2
     * Each of the bits in A or M is shift one place to the right.
     * The bit that was in bit 0 is shifted into the carry flag.
     * Bit 7 is set to zero.
     */
    fn lsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data >>= 1;

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn lsr_accumulator(&mut self) {
        let mut data = self.register_a;

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data >>= 1;

        self.set_register_a(data);
    }

    /**
     * ROL - Rotate Left
     * Move each of the bits in either A or M one place to the left.
     * Bit 0 is filled with the current value of the carry flag whilst the old bit 7 becomes the new carry flag value.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#ROL>
     */
    fn rol(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        let carry_status = self.status.contains(CpuFlags::CARRY);

        // the old bit 7 becomes the new carry flag value
        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data <<= 1;

        // Bit 0 is filled with the current value of the carry flag
        if carry_status {
            data |= 1;
        }

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn rol_accumulator(&mut self) {
        let mut data = self.register_a;

        let carry_status = self.status.contains(CpuFlags::CARRY);

        // the old bit 7 becomes the new carry flag value
        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        data <<= 1;

        // Bit 0 is filled with the current value of the carry flag
        if carry_status {
            data |= 1;
        }

        self.set_register_a(data);
    }

    /**
     * ROR - Rotate Right
     * Move each of the bits in either A or M one place to the right.
     * Bit 7 is filled with the current value of the carry flag whilst the old bit 0 becomes the new carry flag value.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#ROR>
     */
    fn ror(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);

        let carry_status = self.status.contains(CpuFlags::CARRY);

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data >>= 1;
        if carry_status {
            data |= 0b1000_0000;
        }

        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
    }

    fn ror_accumulator(&mut self) {
        let mut data = self.register_a;

        let carry_status = self.status.contains(CpuFlags::CARRY);

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data >>= 1;
        if carry_status {
            data |= 0b1000_0000;
        }

        self.set_register_a(data);
    }

    /**
     * BCC - Branch if Carry Clear
     * If the carry flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BCC>
     */
    fn bcc(&mut self) {
        self.branch(!self.status.contains(CpuFlags::CARRY));
    }

    /**
     * BCS - Branch if Carry Set
     * If the carry flag is set then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BCS>
     */
    fn bcs(&mut self) {
        self.branch(self.status.contains(CpuFlags::CARRY));
    }

    /**
     * BEQ - Branch if Equal
     * If the zero flag is set then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BEQ>
     */
    fn beq(&mut self) {
        self.branch(self.status.contains(CpuFlags::ZERO));
    }

    /**
     * BNE - Branch if Not Equal
     * If the zero flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BNE>
     */
    fn bne(&mut self) {
        self.branch(!self.status.contains(CpuFlags::ZERO));
    }

    /**
     * BVC - Branch if Overflow Clear
     * If the overflow flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BVC>
     */
    fn bvc(&mut self) {
        self.branch(!self.status.contains(CpuFlags::OVERFLOW));
    }

    /**
     * BVS - Branch if Overflow Set
     * If the overflow flag is set then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BVS>
     */
    fn bvs(&mut self) {
        self.branch(self.status.contains(CpuFlags::OVERFLOW));
    }

    /**
     * BMI - Branch if Minus
     * If the negative flag is set then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BMI>
     */
    fn bmi(&mut self) {
        self.branch(self.status.contains(CpuFlags::NEGATIVE));
    }

    /**
     * BPL - Branch if Positive
     * If the negative flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BPL>
     */
    fn bpl(&mut self) {
        self.branch(!self.status.contains(CpuFlags::NEGATIVE));
    }

    /**
     * BIT - Bit Test
     * This instructions is used to test if one or more bits are set in a target memory location.
     * The mask pattern in A is `AND`ed with the value in memory to set or clear the zero flag, but the result is not kept.
     * Bits 7 and 6 of the value from memory are copied into the N and V flags.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#BIT>
     */
    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        let result = self.register_a & data;

        if result == 0 {
            self.set_zero_flag();
        } else {
            self.clear_zero_flag();
        }

        // Set to bit 6 of the memory value
        self.status.set(CpuFlags::OVERFLOW, data & 0b0100_0000 > 0);
        // Set to bit 7 of the memory value
        self.status.set(CpuFlags::NEGATIVE, data & 0b1000_0000 > 0);
    }

    /**
     * CLC - Clear Carry Flag
     * Set the carry flag to zero.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#CLC>
     */
    fn clc(&mut self) {
        self.clear_carry_flag();
    }

    /**
     * SEC - Set Carry Flag
     * Set the carry flag to one.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#SEC>
     */
    fn sec(&mut self) {
        self.set_carry_flag();
    }

    /**
     * CLD - Clear Decimal Mode
     * Sets the decimal mode flag to zero.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#CLD>
     */
    fn cld(&mut self) {
        self.status.remove(CpuFlags::DECIMAL_MODE);
    }

    /**
     * SED - Set Decimal Flag
     * Set the decimal mode flag to one.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#SED>
     */
    fn sed(&mut self) {
        self.status.insert(CpuFlags::DECIMAL_MODE);
    }

    /**
     * CLI - Clear Interrupt Disable
     * Clears the interrupt disable flag allowing normal interrupt requests to be serviced.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#CLI>
     */
    fn cli(&mut self) {
        self.status.remove(CpuFlags::INTERRUPT_DISABLE);
    }

    /**
     * SEI - Set Interrupt Disable
     * Set the interrupt disable flag to one.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#SEI>
     */
    fn sei(&mut self) {
        self.status.insert(CpuFlags::INTERRUPT_DISABLE);
    }

    /**
     * CLV - Clear Overflow Flag
     * Clears the overflow flag.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#CLV>
     */
    fn clv(&mut self) {
        self.status.remove(CpuFlags::OVERFLOW);
    }

    /**
     * CMP - Compare
     * This instruction compares the contents of the accumulator with another memory held value and sets the zero and carry flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#CMP>
     */
    fn cmp(&mut self, mode: &AddressingMode) {
        self.compare(mode, self.register_a);
    }

    /**
     * CPX - Compare X Register
     * This instruction compares the contents of the X register with another memory held value and sets the zero and carry flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#CPX>
     */
    fn cpx(&mut self, mode: &AddressingMode) {
        self.compare(mode, self.register_x);
    }

    /**
     * CPY - Compare Y Register
     * This instruction compares the contents of the Y register with another memory held value and sets the zero and carry flags as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#CPY>
     */
    fn cpy(&mut self, mode: &AddressingMode) {
        self.compare(mode, self.register_y);
    }

    /**
     * JMP - Jump
     * Sets the program counter to the address specified by the operand.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#JMP>
     */
    fn jmp_absolute(&mut self) {
        let mem_address = self.mem_read_u16(self.program_counter);

        self.program_counter = mem_address;
    }

    /**
     * An original 6502 has does not correctly fetch the target address if the indirect vector falls on a page boundary (e.g. $`xxFF` where xx is any value from $`00` to `$FF`).
     * In this case fetches the LSB from $`xxFF` as expected but takes the MSB from `$xx00`.
     * This is fixed in some later chips like the 65SC02 so for compatibility always ensure the indirect vector is not at the end of the page.
     */
    fn jmp_indirect(&mut self) {
        let mem_address = self.mem_read_u16(self.program_counter);

        // 6502 bug mode with with page boundary:
        // if address $3000 contains $40, $30FF contains $80, and $3100 contains $50,
        // the result of JMP ($30FF) will be a transfer of control to $4080 rather than $5080 as you intended
        // i.e. the 6502 took the low byte of the address from $30FF and the high byte from $3000
        let indirect_ref = if mem_address & 0x00FF == 0x00FF {
            let lo = self.mem_read(mem_address);
            let hi = self.mem_read(mem_address & 0xFF00);
            (u16::from(hi)) << 8 | u16::from(lo)
        } else {
            self.mem_read_u16(mem_address)
        };

        self.program_counter = indirect_ref;
    }

    /**
     * JSR - Jump to Subroutine
     * The JSR instruction pushes the address (minus one) of the return point on to the stack and then sets the program counter to the target memory address.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#JSR>
     */
    fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let target_adress = self.mem_read_u16(addr);

        self.stack_push_u16(self.program_counter + 2 - 1);

        self.program_counter = target_adress;
    }

    /**
     * RTS - Return from Subroutine
     * The RTS instruction is used at the end of a subroutine to return to the calling routine. It pulls the program counter (minus one) from the stack.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#RTS>
     */
    fn rts(&mut self) {
        self.program_counter = self.stack_pop_u16() + 1;
    }

    /**
     * PHA - Push Accumulator
     * Pushes a copy of the accumulator on to the stack.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#PHA>
     */
    fn pha(&mut self) {
        self.stack_push(self.register_a);
    }

    /**
     * PLA - Pull Accumulator
     * Pulls an 8 bit value from the stack and into the accumulator.
     * The zero and negative flags are set as appropriate.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#PLA>
     */
    fn pla(&mut self) {
        let data = self.stack_pop();
        self.set_register_a(data);
    }

    /**
     * PHP - Push Processor Status
     * Pushes a copy of the status flags on to the stack.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#PHP>
     */
    fn php(&mut self) {
        let mut flags = self.status.clone();
        flags.insert(CpuFlags::BREAK);
        flags.insert(CpuFlags::BREAK2);
        self.stack_push(flags.bits());
    }

    /**
     * PLP - Pull Processor Status
     * Pulls an 8 bit value from the stack and into the processor flags.
     * The flags will take on new states as determined by the value pulled.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#PLP>
     */
    fn plp(&mut self) {
        self.status = CpuFlags::from_bits_truncate(self.stack_pop());
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    }

    /**
     * RTI - Return from Interrupt
     * The RTI instruction is used at the end of an interrupt processing routine.
     * It pulls the processor flags from the stack followed by the program counter.
     * <https://www.nesdev.org/obelisk-6502-guide/reference.html#RTI>
     */
    fn rti(&mut self) {
        self.status = CpuFlags::from_bits_truncate(self.stack_pop());
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);

        self.program_counter = self.stack_pop_u16();
    }

    fn compare(&mut self, mode: &AddressingMode, register: u8) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);

        if register >= data {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        let result = register.wrapping_sub(data);
        self.update_zero_and_negative_flags(result);
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

    fn set_zero_flag(&mut self) {
        self.status.insert(CpuFlags::ZERO);
    }

    fn clear_zero_flag(&mut self) {
        self.status.remove(CpuFlags::ZERO);
    }

    fn branch(&mut self, condition: bool) {
        if condition {
            #[allow(clippy::cast_possible_wrap)]
            let relative_displacement: i8 = self.mem_read(self.program_counter) as i8;
            #[allow(clippy::cast_sign_loss)]
            let jump_addr = self
                .program_counter
                .wrapping_add(1)
                .wrapping_add(relative_displacement as u16);

            self.program_counter = jump_addr;
        }
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

    #[test]
    fn test_lsr() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x01, 0x4A, 0x00]);

        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
    }

    #[test]
    fn test_rol() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xF0, 0x2A, 0x00]);

        assert_eq!(cpu.register_a, 0xE0);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
    }

    #[test]
    fn test_ror() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x01, 0x6A, 0x00]);

        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
    }

    #[test]
    fn test_branch_if_carry_clear() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![
            0x90, 0x08, 0xA9, 0x01, 0xC9, 0x02, 0xD0, 0x02, 0x85, 0x22, 0x00,
        ]);

        assert_eq!(cpu.register_a, 0x00);
    }

    #[test]
    fn test_branch_if_carry_set() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0x69, 0x02, 0xB0, 0x02, 0xA9, 0x0F, 0x00]);

        assert_eq!(cpu.register_a, 0x01);
    }

    #[test]
    fn test_branch_if_equal() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x00, 0xF0, 0x02, 0xA9, 0xFF, 0x00]);

        assert_eq!(cpu.register_a, 0x00);
    }

    #[test]
    fn test_branch_if_not_equal() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0xD0, 0x02, 0xA9, 0x00, 0x00]);

        assert_eq!(cpu.register_a, 0xFF);
    }

    #[test]
    fn test_branch_if_minus() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0x29, 0xFF, 0x30, 0x02, 0xA9, 0xEE, 0x00]);

        assert_eq!(cpu.register_a, 0xFF);
    }

    #[test]
    fn test_branch_if_positive() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x0F, 0x10, 0x02, 0xA9, 0xEE, 0x00]);

        assert_eq!(cpu.register_a, 0x0F);
    }

    #[test]
    fn test_branch_if_overflow_clear() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0xFF, 0x50, 0x02, 0xA9, 0xEE, 0x00]);

        assert_eq!(cpu.register_a, 0xFF);
    }

    #[test]
    fn test_branch_if_overflow_set() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x40, 0x69, 0x40, 0x70, 0x02, 0xA9, 0xEE, 0x00]);

        assert_eq!(cpu.register_a, 0x80);
    }

    #[test]
    fn test_set_carry_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0x38, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
    }

    #[test]
    fn test_clear_carry_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0x38, 0x18, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
    }

    #[test]
    fn test_set_decimal_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xF8, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::DECIMAL_MODE), true);
    }

    #[test]
    fn test_clear_decimal_mode() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xF8, 0xD8, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::DECIMAL_MODE), false);
    }

    #[test]
    fn test_set_interrupt_disable() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0x78, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::INTERRUPT_DISABLE), true);
    }

    #[test]
    fn test_clear_interrupt_disable() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0x78, 0xA9, 0xFF, 0x58, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::INTERRUPT_DISABLE), false);
    }

    #[test]
    fn test_clear_overflow_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x40, 0x69, 0x40, 0xB8, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::OVERFLOW), false);
    }

    #[test]
    fn test_compare_a_greater() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x05, 0xC9, 0x03, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
    }

    #[test]
    fn test_compare_a_equal() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x05, 0xC9, 0x05, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
    }

    #[test]
    fn test_compare_a_less() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA9, 0x02, 0xC9, 0x05, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), true);
    }

    #[test]
    fn test_compare_x_greater() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA2, 0x05, 0xE0, 0x03, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
    }

    #[test]
    fn test_compare_x_equal() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA2, 0x05, 0xE0, 0x05, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
    }

    #[test]
    fn test_compare_x_less() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA2, 0x02, 0xE0, 0x05, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), true);
    }

    #[test]
    fn test_compare_y_greater() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA0, 0x05, 0xC0, 0x03, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
    }

    #[test]
    fn test_compare_y_equal() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA0, 0x05, 0xC0, 0x05, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), true);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
    }

    #[test]
    fn test_compare_y_less() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA0, 0x02, 0xC0, 0x05, 0x00]);

        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), true);
    }

    #[test]
    fn test_ldx_immediate() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA2, 0x33, 0x00]);

        assert_eq!(cpu.register_x, 0x33);
    }

    #[test]
    fn test_ldy_immediate() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&vec![0xA0, 0x33, 0x00]);

        assert_eq!(cpu.register_y, 0x33);
    }

    #[test]
    fn test_dec() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(&vec![0xCE, 0x10, 0x00]);

        let result = cpu.mem_read(0x10);

        assert_eq!(result, 0x54);
    }

    #[test]
    fn test_dex() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xA2, 0x01, 0xCA, 0x00]);

        assert_eq!(cpu.register_x, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
    }

    #[test]
    fn test_dey() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xA0, 0x01, 0x88, 0x00]);

        assert_eq!(cpu.register_y, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
    }

    #[test]
    fn test_exclusive_or() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xA9, 0x01, 0x49, 0x01, 0x00]);

        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
    }

    #[test]
    fn test_logical_inclusive_or() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xA9, 0x01, 0x09, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x11);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
    }

    #[test]
    fn test_increment_memory() {
        let mut cpu = CPU::new();

        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(&vec![0xE6, 0x10, 0x00]);

        let result = cpu.mem_read(0x10);

        assert_eq!(result, 0x56);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
    }

    #[test]
    fn test_increment_y() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xA0, 0x01, 0xC8, 0x00]);

        assert_eq!(cpu.register_y, 0x02);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
    }

    #[test]
    fn test_increment_y_overflow() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xA0, 0xFF, 0xC8, 0x00]);

        assert_eq!(cpu.register_y, 0x00);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), true);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), false);
        assert_eq!(cpu.status.contains(CpuFlags::OVERFLOW), false);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
    }

    #[test]
    fn test_stack_accumulator() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xA9, 0xFF, 0x48, 0xA9, 0x33, 0x68, 0x00]);

        assert_eq!(cpu.register_a, 0xFF);
        assert_eq!(cpu.status.contains(CpuFlags::ZERO), false);
        assert_eq!(cpu.status.contains(CpuFlags::NEGATIVE), true);
        assert_eq!(cpu.status.contains(CpuFlags::OVERFLOW), false);
        assert_eq!(cpu.status.contains(CpuFlags::CARRY), false);
    }

    //#[test]
    //fn test_jump_absolute() {
    //    let mut cpu = CPU::new();
    //
    //    cpu.load_and_run(&vec![0x4C, 0x30, 0xFF]);
    //
    //    assert_eq!(cpu.program_counter, 0xFF30);
    //}

    /*#[test]
    fn test_jump_indirect_bug() {
        let mut cpu = CPU::new();

        cpu.mem_write(0x3000, 0x40);
        cpu.mem_write(0x30FF, 0x80);
        cpu.mem_write(0x3100, 0x50);

        // JMP ($30FF)
        cpu.load_and_run(&vec![0x6C, 0xFF, 0x30]);

        assert_eq!(cpu.program_counter, 0x4080);
    }*/

    //#[test]
    //fn test_bit_test() {
    //let mut cpu = CPU::new();
    //cpu.load_and_run(&vec![0xA9, 0xFF, 0xD0, 0x02, 0xA9, 0x00, 0x00]);

    //assert_eq!(cpu.register_a, 0xFF);
    //assert_eq!(cpu.program_counter, 0x8007);
    //}
}
