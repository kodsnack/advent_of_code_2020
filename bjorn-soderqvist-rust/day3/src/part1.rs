use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn get_n_trees() -> i32 {
    let filename = "./day3/src/map.txt";
    let mut trees = 0;
    let mut x_pos = 0;

    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);

    for (_index, line) in reader.lines().enumerate() {
        let line = line.unwrap();
        let tile = line.chars().nth(x_pos).unwrap();
        if tile == '#' {
            trees = trees + 1;
        }
        x_pos = (x_pos + 3) % line.len();
    }
    trees
}
