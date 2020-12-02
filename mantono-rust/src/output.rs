use crate::cfg::{Config, Part};
use crate::fmt::write;
use termcolor::{Color, StandardStream};

pub fn print_out(output: String, cfg: &Config) {
    let mut stdout = StandardStream::stdout(cfg.use_colors);
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
