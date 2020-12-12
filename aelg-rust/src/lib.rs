mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
pub mod error;
pub mod parser;
pub mod readinput;
pub mod testhelper;
pub mod vm;

pub type AOCResult<T> = Result<T, error::AOCError>;
pub use error::AOCError;

pub fn solveday(
    day: u32,
    input: &[&str],
) -> Result<(Option<String>, Option<String>), error::AOCError> {
    match day {
        1 => {
            let r = day01::parse(input);
            r.map(day01::solve)
        }
        2 => {
            let r = day02::parse(input);
            r.map(day02::solve)
        }
        3 => {
            let r = day03::parse(input);
            r.map(day03::solve)
        }
        4 => {
            let r = day04::parse(input);
            r.map(day04::solve)
        }
        5 => {
            let r = day05::parse(input);
            r.map(day05::solve)
        }
        6 => {
            let r = day06::parse(input);
            r.map(day06::solve)
        }
        7 => {
            let r = day07::parse(input);
            r.map(day07::solve)
        }
        8 => {
            let r = day08::parse(input);
            r.map(day08::solve)
        }
        9 => {
            let r = day09::parse(input);
            r.map(day09::solve)
        }
        10 => {
            let r = day10::parse(input);
            r.map(day10::solve)
        }
        11 => {
            let r = day11::parse(input);
            r.map(day11::solve)
        }
        12 => {
            let r = day12::parse(input);
            r.map(day12::solve)
        }
        _ => Ok((Some("No solution for this day yet.".into()), None)),
    }
}
