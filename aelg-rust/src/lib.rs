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
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
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
        1 => day01::parse(input).map(day01::solve),
        2 => day02::parse(input).map(day02::solve),
        3 => day03::parse(input).map(day03::solve),
        4 => day04::parse(input).map(day04::solve),
        5 => day05::parse(input).map(day05::solve),
        6 => day06::parse(input).map(day06::solve),
        7 => day07::parse(input).map(day07::solve),
        8 => day08::parse(input).map(day08::solve),
        9 => day09::parse(input).map(day09::solve),
        10 => day10::parse(input).map(day10::solve),
        11 => day11::parse(input).map(day11::solve),
        12 => day12::parse(input).map(day12::solve),
        13 => day13::parse(input).map(day13::solve),
        14 => day14::parse(input).map(day14::solve),
        15 => day15::parse(input).map(day15::solve),
        16 => day16::parse(input).map(day16::solve),
        17 => day17::parse(input).map(day17::solve),
        18 => day18::parse(input).map(day18::solve),
        19 => day19::parse(input).map(day19::solve),
        20 => day20::parse(input).map(day20::solve),
        21 => day21::parse(input).map(day21::solve),
        22 => day22::parse(input).map(day22::solve),
        23 => day23::parse(input).map(day23::solve),
        24 => day24::parse(input).map(day24::solve),
        _ => Ok((Some("No solution for this day yet.".into()), None)),
    }
}
