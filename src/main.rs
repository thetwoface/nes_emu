#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::similar_names, clippy::module_name_repetitions)]

use nes_emu::Config;

fn main() {
    let config = Config::build(std::env::args()).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {err}");
        std::process::exit(1);
    });

    if let Err(e) = nes_emu::run(config) {
        eprintln!("Application error: {e}");
        std::process::exit(1);
    }
}
