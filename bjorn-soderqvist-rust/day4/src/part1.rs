use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
struct Identification {
    byr: bool,
    cid: bool,
    ecl: bool,
    eyr: bool,
    hcl: bool,
    hgt: bool,
    iyr: bool,
    pid: bool,
}

fn check_fields(s: &str) -> Identification {
    let mut id = Identification {
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
        let has_data = val.len() > 0; // this is true in my data, but why not be careful
        match key {
            "byr" => id.byr = has_data,
            "cid" => id.cid = has_data,
            "ecl" => id.ecl = has_data,
            "eyr" => id.eyr = has_data,
            "hcl" => id.hcl = has_data,
            "hgt" => id.hgt = has_data,
            "iyr" => id.iyr = has_data,
            "pid" => id.pid = has_data,
            _ => println!("Unknown key {}", key),
        }
    }
    return id;
}

fn validate(s: &str) -> bool {
    let fields = check_fields(&s);
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
                    _ => 0,
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
            _ => 0,
        };
    }
    valid
}
