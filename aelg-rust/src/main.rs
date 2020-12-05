use aelg_rust as lib;
use lib::readinput::get_input_from_file;
use lib::solveday;
use std::env;

fn print_part(n: i32, a: Option<String>) {
    match a {
        Some(x) => println!("Part {}: {}", n, x),
        None => println!("Part {}: No result", n),
    }
}

fn run_for_day(day: u32) {
    let lines: lib::AOCResult<Vec<String>> = get_input_from_file(day);
    match lines {
        Ok(v) => {
            let w: Vec<&str> = v.iter().map(|l| l.as_str()).collect();
            match solveday(day, &w) {
                Ok((a, b)) => {
                    print_part(1, a);
                    print_part(2, b);
                }
                Err(e) => println!("Error: {}", e),
            }
        }
        Err(e) => println!("Failed to read input: {}", e),
    }
}

fn main() {
    if let Some(day) = env::args().nth(1) {
        if let Ok(day) = day.parse::<u32>() {
            run_for_day(day);
        } else {
            println!("Invalid day!")
        }
    } else {
        println!("Usage: cargo run <day>\nInputs should be in ./inputs/dayX.txt")
    }
}
