#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::similar_names, clippy::module_name_repetitions)]

extern crate sdl2;
//https://blog.logrocket.com/using-sdl2-bindings-rust/

pub mod bus;
pub mod cartridge;
pub mod cpu;
pub mod opcodes;
pub mod ppu;
pub mod trace;

use bus::Bus;
use cartridge::Rom;
use cpu::CPU;
use trace::trace;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate bitflags;

fn main() -> Result<(), String> {
    // init sdl2
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let window = video_subsystem
        .window("NesEMU", 32 * 10, 32 * 10)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window
        .into_canvas()
        .present_vsync()
        .build()
        .map_err(|e| e.to_string())?;

    canvas.set_scale(10.0, 10.0).map_err(|e| e.to_string())?;

    //load the game
    let bytes: Vec<u8> = std::fs::read("nestest.nes").map_err(|e| e.to_string())?;
    let rom = Rom::new(&bytes).map_err(|e| e.to_string())?;
    let bus = Bus::new(rom);
    let mut cpu = CPU::new(bus);

    cpu.reset();
    cpu.program_counter = 0xC000;

    // run the game cycle
    cpu.run_with_callback(move |cpu| {
        println!("{}", trace(cpu));
    });

    Ok(())
}
