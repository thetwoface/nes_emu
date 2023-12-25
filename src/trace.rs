use crate::cpu::AddressingMode;
use crate::cpu::Mem;
use crate::cpu::CPU;
use crate::opcodes;
use std::collections::HashMap;

/// # Panics
///
/// May panic if opcode is not recognized
#[allow(clippy::too_many_lines)]
#[must_use]
pub fn trace(cpu: &mut CPU) -> String {
    let opcodes: &HashMap<u8, &'static opcodes::OpCode> = &opcodes::OPCODES_MAP;

    let code = cpu.mem_read(cpu.program_counter);
    let opcode = opcodes.get(&code).unwrap();

    let begin = cpu.program_counter;
    let mut hex_dump = vec![];
    hex_dump.push(code);

    let (mem_addr, stored_value) = match opcode.mode {
        AddressingMode::Immediate | AddressingMode::NoneAddressing => (0, 0),
        _ => {
            let (addr, _) = cpu.get_absolute_address(&opcode.mode, begin + 1);
            (addr, cpu.mem_read(addr))
        }
    };

    let tmp = match opcode.len {
        1 => match opcode.code {
            0x0A | 0x4A | 0x2A | 0x6A => "A ".to_string(),
            _ => String::new(),
        },
        2 => {
            let address = cpu.mem_read(begin + 1);
            hex_dump.push(address);

            match opcode.mode {
                AddressingMode::Immediate => format!("#${address:02x}"),
                AddressingMode::ZeroPage => format!("${mem_addr:02x} = {stored_value:02x}"),
                AddressingMode::ZeroPageX => {
                    format!("${address:02x},X @ {mem_addr:02x} = {stored_value:02x}")
                }
                AddressingMode::ZeroPageY => {
                    format!("${address:02x},Y @ {mem_addr:02x} = {stored_value:02x}")
                }
                AddressingMode::IndirectX => format!(
                    "(${:02x},X) @ {:02x} = {:04x} = {:02x}",
                    address,
                    (address.wrapping_add(cpu.register_x)),
                    mem_addr,
                    stored_value
                ),
                AddressingMode::IndirectY => format!(
                    "(${:02x}),Y = {:04x} @ {:04x} = {:02x}",
                    address,
                    (mem_addr.wrapping_sub(u16::from(cpu.register_y))),
                    mem_addr,
                    stored_value
                ),
                AddressingMode::NoneAddressing => {
                    // assuming local jumps: BNE, BVS, etc....
                    #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
                    let address: usize =
                        (begin as usize + 2).wrapping_add((address as i8) as usize);
                    format!("${address:04x}")
                }

                _ => panic!(
                    "unexpected addressing mode {:?} has ops-len 2. code {:02x}",
                    opcode.mode, opcode.code
                ),
            }
        }
        3 => {
            let address_lo = cpu.mem_read(begin + 1);
            let address_hi = cpu.mem_read(begin + 2);
            hex_dump.push(address_lo);
            hex_dump.push(address_hi);

            let address = cpu.mem_read_u16(begin + 1);

            match opcode.mode {
                AddressingMode::NoneAddressing => {
                    if opcode.code == 0x6c {
                        //jmp indirect
                        let jmp_addr = if address & 0x00FF == 0x00FF {
                            let lo = cpu.mem_read(address);
                            let hi = cpu.mem_read(address & 0xFF00);
                            u16::from(hi) << 8 | u16::from(lo)
                        } else {
                            cpu.mem_read_u16(address)
                        };

                        // let jmp_addr = cpu.mem_read_u16(address);
                        format!("(${address:04x}) = {jmp_addr:04x}")
                    } else {
                        format!("${address:04x}")
                    }
                }
                AddressingMode::Absolute => format!("${mem_addr:04x} = {stored_value:02x}"),
                AddressingMode::AbsoluteX => {
                    format!("${address:04x},X @ {mem_addr:04x} = {stored_value:02x}")
                }
                AddressingMode::AbsoluteY => {
                    format!("${address:04x},Y @ {mem_addr:04x} = {stored_value:02x}")
                }
                _ => panic!(
                    "unexpected addressing mode {:?} has ops-len 3. code {:02x}",
                    opcode.mode, opcode.code
                ),
            }
        }
        _ => String::new(),
    };

    let hex_str = hex_dump
        .iter()
        .map(|z| format!("{z:02x}"))
        .collect::<Vec<String>>()
        .join(" ");
    let asm_str = format!(
        "{:04x}  {:8} {: >4} {}",
        begin, hex_str, opcode.mnemonic, tmp
    )
    .trim()
    .to_string();

    format!(
        "{:47} A:{:02x} X:{:02x} Y:{:02x} P:{:02x} SP:{:02x}",
        asm_str, cpu.register_a, cpu.register_x, cpu.register_y, cpu.status, cpu.stack_pointer,
    )
    .to_ascii_uppercase()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::bus::Bus;
    use crate::cartridge::test::build_test_rom;

    #[test]
    fn test_format_trace() {
        let mut bus = Bus::new(build_test_rom());
        bus.mem_write(100, 0xa2);
        bus.mem_write(101, 0x01);
        bus.mem_write(102, 0xca);
        bus.mem_write(103, 0x88);
        bus.mem_write(104, 0x00);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_a = 1;
        cpu.register_x = 2;
        cpu.register_y = 3;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
            result[2]
        );
    }

    #[test]
    fn test_format_mem_access() {
        let mut bus = Bus::new(build_test_rom());
        // ORA ($33), Y
        bus.mem_write(100, 0x11);
        bus.mem_write(101, 0x33);

        //data
        bus.mem_write(0x33, 00);
        bus.mem_write(0x34, 04);

        //target cell
        bus.mem_write(0x400, 0xAA);

        let mut cpu = CPU::new(bus);
        cpu.program_counter = 0x64;
        cpu.register_y = 0;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(trace(cpu));
        });
        assert_eq!(
            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
            result[0]
        );
    }
}
