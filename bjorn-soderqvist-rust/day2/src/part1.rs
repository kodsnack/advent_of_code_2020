use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn validate(data: &String) -> bool {
    let mut iter = data.split_whitespace();
    let mut range_iter = iter.next().unwrap().split("-");
    let min_length: i32 = range_iter.next().unwrap().parse().unwrap();
    let max_length: i32 = range_iter.next().unwrap().parse().unwrap();
    let required_char = iter.next().unwrap().chars().nth(0).unwrap();
    let password = iter.next().unwrap().chars();

    let mut n = 0;
    for c in password {
        if c == required_char {
            n = n + 1;
        }
    }
    return n <= max_length && n >= min_length;
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