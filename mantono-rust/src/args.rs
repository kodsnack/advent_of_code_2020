use clap::{App, Arg, ArgMatches};

pub fn args<'a>() -> ArgMatches<'a> {
    let verbosity = Arg::with_name("verbosity")
        .takes_value(true)
        .default_value("1")
        .validator(|n: String| {
            let range = 0u8..=5u8;
            let n: u8 = n.parse::<u8>().unwrap();
            if range.contains(&n) {
                Ok(())
            } else {
                Err("Invalid value".to_string())
            }
        })
        .short("v")
        .long("verbosity")
        .help("Set verbosity level, 0 - 5")
        .long_help("Set the verbosity level, from 0 (least amount of output) to 5 (most verbose). Note that logging level configured via RUST_LOG overrides this setting.");

    let colors = Arg::with_name("colors")
        .long("colors")
        .short("C")
        .takes_value(true)
        .default_value("auto")
        .possible_values(&["auto", "never", "always", "ansi"])
        .value_name("setting")
        .help("Enable/disable colors")
        .long_help("Enable or disable output with colors. By default colors will be used if the terminal seems to support colors.");

    let day = Arg::with_name("day")
        .takes_value(true)
        .short("d")
        .long("day")
        .help("Day of Advent of Code to run")
        .validator(validate_day);

    let part = Arg::with_name("part")
        .takes_value(true)
        .short("p")
        .long("part")
        .possible_values(&["1", "2"])
        .help("Part of the puzzle for the day to run");

    let year = Arg::with_name("year")
        .takes_value(true)
        .short("y")
        .long("year")
        .help("Which Advent of Code year to run for");

    let session_token = Arg::with_name("token")
    .takes_value(true)
    .short("t")
    .long("token")
    .env("AOC_TOKEN")
    .help("Advent of Code session token")
    .long_help("Advent of Code session token that is used to fetch puzzle inputs if they are not already present on disk");

    let args: ArgMatches = App::new(crate_name!())
        .about("DESCRIPTION")
        .version(crate_version!())
        .author(crate_authors!())
        .arg(verbosity)
        .arg(colors)
        .arg(year)
        .arg(day)
        .arg(part)
        .arg(session_token)
        .get_matches();

    args
}

fn validate_day(input: String) -> Result<(), String> {
    match input.parse::<u16>() {
        Ok(day) => match day {
            1..=25 => Ok(()),
            _ => Err(format!("Value not a valid day (1-25): {}", day)),
        },
        Err(e) => Err(e.to_string()),
    }
}
