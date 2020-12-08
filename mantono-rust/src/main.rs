#[macro_use]
extern crate clap;
extern crate lazy_static;

use cfg::Puzzle;
use cfg::{Config, Part};
use input::get_input;
use logger::setup_logging;
use output::msg;
use output::print_out;

mod args;
mod cfg;
mod days;
mod fmt;
mod input;
mod logger;
mod output;

fn main() -> Result<(), String> {
    let cfg: Config = Config::from_args(args::args());
    setup_logging(cfg.verbosity_level);
    msg(&cfg);
    let puzzle_input: String = get_input(&cfg.puzzle, &cfg.session_token)?;
    let start = std::time::Instant::now();
    let output: String = run_puzzle(&cfg.puzzle, puzzle_input);
    let end = std::time::Instant::now();
    let elapsed = end.duration_since(start);
    print_out(output, elapsed, &cfg);

    Ok(())
}

fn run_puzzle(puzzle: &Puzzle, input: String) -> String {
    match (puzzle.day, &puzzle.part) {
        (1, Part::One) => days::day1::first(input),
        (1, Part::Two) => days::day1::second(input),
        (2, Part::One) => days::day2::first(input),
        (2, Part::Two) => days::day2::second(input),
        (3, Part::One) => days::day3::first(input),
        (3, Part::Two) => days::day3::second(input),
        (4, Part::One) => days::day4::first(input),
        (4, Part::Two) => days::day4::second(input),
        _ => panic!("Not supported"),
    }
}
