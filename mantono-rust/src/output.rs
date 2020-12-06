use std::time::Duration;

use crate::cfg::{Config, Part};
use crate::fmt::write;
use termcolor::{Color, StandardStream};

pub fn print_out(output: String, elapsed_time: Duration, cfg: &Config) {
    let mut stdout = StandardStream::stdout(cfg.use_colors);
    let micro_seconds: u128 = elapsed_time.as_micros();
    let milli_seconds: u128 = elapsed_time.as_millis();
    let time: String = format!("Completed in {} μs ({} ms)\n", micro_seconds, milli_seconds);
    write(&mut stdout, &time, Some(Color::Rgb(128, 128, 128)));

    write(
        &mut stdout,
        "Puzzle output: ",
        Some(Color::Rgb(128, 128, 128)),
    );
    write(&mut stdout, &output, Some(Color::Yellow));
    write(&mut stdout, "\n", None);
}

pub fn msg(cfg: &Config) {
    let mut stdout = StandardStream::stdout(cfg.use_colors);
    write(&mut stdout, "🎄 Running Advent of Code", Some(Color::Green));
    write(
        &mut stdout,
        &format!(" {} ", cfg.puzzle.year),
        Some(Color::Red),
    );
    write(&mut stdout, "day", Some(Color::Green));
    write(
        &mut stdout,
        &format!(" {} ", cfg.puzzle.day),
        Some(Color::Red),
    );
    write(&mut stdout, "part", Some(Color::Green));
    let part: &str = match cfg.puzzle.part {
        Part::One => "one",
        Part::Two => "two",
    };
    write(&mut stdout, &format!(" {}\n", part), Some(Color::Red));
}
