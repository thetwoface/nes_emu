#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::similar_names)]

extern crate sdl2;
//https://blog.logrocket.com/using-sdl2-bindings-rust/

pub mod bus;
pub mod cartridge;
pub mod cpu;
pub mod opcodes;
pub mod trace;

use bus::Bus;
use cartridge::Rom;
//use cpu::Mem;
use cpu::CPU;
//use sdl2::event::Event;
//use sdl2::keyboard::Keycode;
//use sdl2::pixels::Color;
//use sdl2::pixels::PixelFormatEnum;
//use sdl2::EventPump;
use trace::trace;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate bitflags;

/*fn color(byte: u8) -> Color {
    match byte {
        0 => sdl2::pixels::Color::BLACK,
        1 => sdl2::pixels::Color::WHITE,
        2 | 9 => sdl2::pixels::Color::GREY,
        3 | 10 => sdl2::pixels::Color::RED,
        4 | 11 => sdl2::pixels::Color::GREEN,
        5 | 12 => sdl2::pixels::Color::BLUE,
        6 | 13 => sdl2::pixels::Color::MAGENTA,
        7 | 14 => sdl2::pixels::Color::YELLOW,
        _ => sdl2::pixels::Color::CYAN,
    }
}*/

/*fn read_screen_state(cpu: &CPU, frame: &mut [u8; 32 * 3 * 32]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x0200u16..0x600u16 {
        let color_idx = cpu.mem_read(i);
        let (b1, b2, b3) = color(color_idx).rgb();
        if frame[frame_idx] != b1 || frame[frame_idx + 1] != b2 || frame[frame_idx + 2] != b3 {
            frame[frame_idx] = b1;
            frame[frame_idx + 1] = b2;
            frame[frame_idx + 2] = b3;
            update = true;
        }
        frame_idx += 3;
    }
    update
}*/

/*fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => std::process::exit(0),
            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => {
                cpu.mem_write(0xff, 0x77);
            }
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => {
                cpu.mem_write(0xff, 0x73);
            }
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => {
                cpu.mem_write(0xff, 0x61);
            }
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => {
                cpu.mem_write(0xff, 0x64);
            }
            _ => { /* do nothing */ }
        }
    }
}*/

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

    //let mut event_pump = sdl_context.event_pump()?;
    canvas.set_scale(10.0, 10.0).map_err(|e| e.to_string())?;

    //let creator = canvas.texture_creator();
    //let mut texture = creator
    //    .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
    //    .map_err(|e| e.to_string())?;

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
