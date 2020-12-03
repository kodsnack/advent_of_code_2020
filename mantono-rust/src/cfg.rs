use crate::fmt::color_choice;
use chrono::Datelike;
use clap::ArgMatches;
use termcolor::ColorChoice;

pub struct Config {
    pub verbosity_level: u8,
    pub use_colors: ColorChoice,
    pub puzzle: Puzzle,
    pub session_token: Option<String>,
}

#[derive(Debug)]
pub struct Puzzle {
    pub year: u16,
    pub day: u8,
    pub part: Part,
}

impl std::fmt::Display for Puzzle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let part: &str = match self.part {
            Part::One => "one",
            Part::Two => "two",
        };
        write!(f, "{} day {} part {}", self.year, self.day, part)
    }
}

#[derive(Debug)]
pub enum Part {
    One,
    Two,
}

impl Config {
    pub fn from_args(args: ArgMatches) -> Config {
        let aoc_tz = chrono::FixedOffset::west(5 * 3600);
        let local = chrono::Local::now();
        let aoc_date = local.with_timezone(&aoc_tz).date();

        let year = if aoc_date.month() == 12 {
            aoc_date.year()
        } else {
            aoc_date.year() - 1
        };

        let day = if aoc_date.month() == 12 {
            aoc_date.day()
        } else {
            1
        };

        let verbosity_level: u8 = args.value_of("verbosity").unwrap().parse::<u8>().unwrap();
        let use_colors: ColorChoice = color_choice(args.value_of("colors").unwrap());
        let year: u16 = args
            .value_of("year")
            .map(|y| y.parse::<u16>().unwrap())
            .unwrap_or(year as u16);

        let day: u8 = args
            .value_of("day")
            .map(|d| d.parse::<u8>().unwrap())
            .unwrap_or(day as u8);

        let part: Part = match args.value_of("part") {
            Some("1") | None => Part::One,
            Some("2") => Part::Two,
            _ => panic!("Will not happen..."),
        };

        let puzzle = Puzzle { year, day, part };

        let session_token: Option<String> = args.value_of("token").map(|s| s.to_owned());

        Config {
            verbosity_level,
            use_colors,
            puzzle,
            session_token,
        }
    }
}
