#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::similar_names)]

pub mod cpu;
pub mod opcodes;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate bitflags;

fn main() {
    println!("Hello, world!");
}
