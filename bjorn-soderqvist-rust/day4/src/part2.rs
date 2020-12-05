use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct IdentificationValidation {
    byr: bool,
    cid: bool,
    ecl: bool,
    eyr: bool,
    hcl: bool,
    hgt: bool,
    iyr: bool,
    pid: bool,
}

// needs to be a 6 digit hex value prefixed by #
fn valid_hair_color(s: &str) -> bool {
    let len = s.len();
    match len {
        7 => {
            let mut chars = s.chars();
            let first = chars.nth(0).unwrap();
            match first {
                '#' => {
                    let mut hex_val = s.to_string();
                    hex_val.remove(0);
                    hex_val.chars().all(|x| x.is_digit(16))
                },
                _ => false,
            }
        },
        _ => false,
    }
}

fn validate_height(s: &str) -> bool {
    if s.ends_with("cm") {
        let num:i32 = s.split("cm").next().unwrap().parse().unwrap();
        return num >= 150 && num <= 193;
    }
    if s.ends_with("in") {
        let num:i32 = s.split("in").next().unwrap().parse().unwrap();
        return num >= 59 && num <= 76;
    }
    false
}

fn valid_passport_id(s: &str) -> bool {
    let len = s.len();
    match len {
        9 => s.chars().all(|x| x.is_digit(10)),
        _ => false,
    }
}

fn valid_year_within(year: i32, min: i32, max: i32) -> bool {
    year >= min && year <= max
}

fn valid_eye_color(s: &str) -> bool {
    match s {
        "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true,
        _ => false,
    }
}

fn check_fields(s: &str) -> IdentificationValidation {
    let mut id = IdentificationValidation {
        byr: false,
        cid: false,
        ecl: false,
        eyr: false,
        hcl: false,
        hgt: false,
        iyr: false,
        pid: false,
    };

    for keyval in s.split_whitespace() {
        let mut pair = keyval.split(":");
        let key = pair.next().unwrap();
        let val = pair.next().unwrap();
        match key {
            "byr" => id.byr = valid_year_within(val.parse().unwrap(), 1920, 2002),
            "cid" => id.cid = true, // This will be ignored
            "ecl" => id.ecl = valid_eye_color(val),
            "eyr" => id.eyr = valid_year_within(val.parse().unwrap(), 2020, 2030),
            "hcl" => id.hcl = valid_hair_color(val),
            "hgt" => id.hgt = validate_height(val),
            "iyr" => id.iyr = valid_year_within(val.parse().unwrap(), 2010, 2020),
            "pid" => id.pid = valid_passport_id(val),
            _ => println!("Unknown key {}", key),
        }
    }
    return id;
}

fn validate(s: &str) -> bool {
    let fields = check_fields(s);
    return fields.byr &&
    // fields.cid,
    fields.ecl &&
    fields.eyr &&
    fields.hcl &&
    fields.hgt &&
    fields.iyr &&
    fields.pid;
}

pub fn get_n_valid_ids() -> i32 {
    let file = File::open("./day4/src/input.txt").unwrap();
    let reader = BufReader::new(file);
    let mut valid = 0;
    let mut s = "".to_string();
    for (_index, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        let empty_line = line.len() == 0;
        match empty_line {
            true => {
                let is_valid = validate(&s);
                valid = valid + match is_valid {
                    true => 1,
                    false => 0,
                };
                s = "".to_string();
            },
            _ => s = format!("{} {}", s, line),
        }
    }
    // Lololol
    if s.len() > 0 {
        println!("Almost missed the last entry");
        let is_valid = validate(&s);
        valid = valid + match is_valid {
            true => 1,
            false => 0,
        };
    }
    valid
}
