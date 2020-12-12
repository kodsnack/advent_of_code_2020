// mod part2;
// mod rotate;
mod lib;

fn main() {
    let data = lib::parse_file(include_str!("input.txt"));
    let start = lib::Location {
        x: 0,
        y: 0,
        direction: 'E', // default
    };
    let mut ship = lib::Location {
        x: 0,
        y: 0,
        direction: 'E', // default
    };
    data.iter().for_each(|instruction|ship = lib::follow_instruction(&ship, instruction));
    let distance = lib::manhattan_distance(&start, &ship);
    println!("Day 12 Part 1: {}", distance);
}
