#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::similar_names, clippy::module_name_repetitions)]

extern crate sdl2;
//https://blog.logrocket.com/using-sdl2-bindings-rust/

pub mod bus;
pub mod cartridge;
pub mod cpu;
pub mod opcodes;
pub mod ppu;
pub mod render;
pub mod trace;

//use bus::Bus;
use cartridge::Rom;
//use cpu::CPU;
use render::frame::Frame;
use sdl2::{event::Event, keyboard::Keycode, pixels::PixelFormatEnum};
//use trace::trace;

use crate::render::palette;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate bitflags;

/*fn show_tile(chr_rom: &Vec<u8>, bank: usize, tile_n: usize) -> Frame {
    assert!(bank <= 1);

    let mut frame = Frame::new();
    let bank = (bank * 0x1000) as usize;

    let tile = &chr_rom[(bank + tile_n * 16)..=(bank + tile_n * 16 + 15)];

    for y in 0..=7 {
        let mut upper = tile[y];
        let mut lower = tile[y + 8];

        for x in (0..=7).rev() {
            let value = (1 & upper) << 1 | (1 & lower);
            upper = upper >> 1;
            lower = lower >> 1;
            let rgb = match value {
                0 => palette::SYSTEM_PALLETE[0x01],
                1 => palette::SYSTEM_PALLETE[0x23],
                2 => palette::SYSTEM_PALLETE[0x27],
                4 => palette::SYSTEM_PALLETE[0x30],
                _ => panic!("CAN'T GET COLOR"),
            };
            frame.set_pixel(x, y, rgb);
        }
    }

    frame
}*/

fn show_tile_bank(chr_rom: &Vec<u8>, bank: usize) -> Frame {
    assert!(bank <= 1);

    let mut frame = Frame::new();
    let mut tile_y = 0;
    let mut tile_x = 0;
    let bank = (bank * 0x1000) as usize;

    for tile_n in 0..255 {
        if tile_n != 0 && tile_n % 20 == 0 {
            tile_y += 10;
            tile_x = 0;
        }

        let tile = &chr_rom[(bank + tile_n * 16)..=(bank + tile_n * 16 + 15)];

        for y in 0..=7 {
            let mut upper = tile[y];
            let mut lower = tile[y + 8];

            for x in (0..=7).rev() {
                let value = (1 & upper) << 1 | (1 & lower);
                upper = upper >> 1;
                lower = lower >> 1;
                let rgb = match value {
                    0 => palette::SYSTEM_PALLETE[0x01],
                    1 => palette::SYSTEM_PALLETE[0x23],
                    2 => palette::SYSTEM_PALLETE[0x27],
                    3 => palette::SYSTEM_PALLETE[0x30],
                    _ => panic!("can't be"),
                };
                frame.set_pixel(tile_x + x, tile_y + y, rgb)
            }
        }

        tile_x += 10;
    }

    frame
}

fn main() -> Result<(), String> {
    // init sdl2
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let window = video_subsystem
        .window("Tile Viewer", 256 * 3, 240 * 3)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window
        .into_canvas()
        .present_vsync()
        .build()
        .map_err(|e| e.to_string())?;

    let mut event_pump = sdl_context.event_pump().map_err(|e| e.to_string())?;

    canvas.set_scale(3.0, 3.0).map_err(|e| e.to_string())?;

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
        .map_err(|e| e.to_string())?;

    //load the game
    let bytes: Vec<u8> = std::fs::read("Alter_Ego.nes").map_err(|e| e.to_string())?;
    let rom = Rom::new(&bytes).map_err(|e| e.to_string())?;

    /*let bus = Bus::new(rom);
    let mut cpu = CPU::new(bus);

    cpu.reset();
    cpu.program_counter = 0xC000;*/

    let right_bank = show_tile_bank(&rom.chr_rom, 0);

    texture
        .update(None, &right_bank.data, 256 * 3)
        .map_err(|e| e.to_string())?;
    canvas
        .copy(&texture, None, None)
        .map_err(|e| e.to_string())?;
    canvas.present();

    // run the game cycle
    /*cpu.run_with_callback(move |cpu| {
        println!("{}", trace(cpu));
    });*/

    loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                _ => { /* do nothing */ }
            }
        }
    }

    //Ok(())
}
