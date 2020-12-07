use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use std::convert::TryInto;

fn is_char_at_position(required_char: char, pos: i32, password: &str) -> bool {
    let base0pos = (pos - 1).try_into().unwrap();
    let mut password_chars = password.chars();
    if base0pos > password.len() {
        println!("out of bounds, pos {} len {}", base0pos, password.len());
        return false;
    }
    let compare = password_chars.nth(base0pos).unwrap();
    return compare == required_char;
}

fn validate(data: &String) -> bool {
    let mut iter = data.split_whitespace();
    let mut range_iter = iter.next().unwrap().split("-");
    let position1: i32 = range_iter.next().unwrap().parse().unwrap();
    let position2: i32 = range_iter.next().unwrap().parse().unwrap();
    let required_char = iter.next().unwrap().chars().nth(0).unwrap();
    let password = iter.next().unwrap();

    let valid1 = is_char_at_position(required_char, position1, &password);
    let valid2 = is_char_at_position(required_char, position2, &password);
    if valid1 && valid2 {
        return false;
    }
    if !valid1 && !valid2 {
        return false;
    }
    return true;
}

pub fn get_n_valid() -> i32 {
    let mut sum = 0;
    if let Ok(lines) = read_lines("./day2/src/data.txt") {
        for line in lines {
            if let Ok(data) = line {
                let result = validate(&data);
                if result {
                    sum = sum + 1;
                }
            }
        }
        return sum;
    }
    panic!("Can't read the lines!");
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
