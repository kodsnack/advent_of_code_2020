mod part1;
mod part2;

fn main() {
    let part1result = part1::get_n_valid_ids();
    println!("Day 4 part 1: {}", part1result);
    let part2result = part2::get_n_valid_ids();
    println!("Day 4 part 2: {}", part2result);
}
