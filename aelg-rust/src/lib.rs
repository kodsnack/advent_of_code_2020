mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
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
    if day == 1 {
        let r = day01::parse(input);
        return r.map(day01::solve);
    }
    if day == 2 {
        let r = day02::parse(input);
        return r.map(day02::solve);
    }
    if day == 3 {
        let r = day03::parse(input);
        return r.map(day03::solve);
    }
    if day == 4 {
        let r = day04::parse(input);
        return r.map(day04::solve);
    }
    if day == 5 {
        let r = day05::parse(input);
        return r.map(day05::solve);
    }
    Ok((None, None))
}
