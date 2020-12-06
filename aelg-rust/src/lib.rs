mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
pub mod error;
pub mod parser;
pub mod readinput;
pub mod testhelper;

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
        _ => Ok((Some("No solution for this day yet.".into()), None)),
    }
}
