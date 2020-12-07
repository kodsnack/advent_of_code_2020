mod part1;
mod part2;

fn main() {
    let part1result = part1::get_n_trees();
    println!("Day 3 part 1: {}", part1result);
    let part2result = part2::get_n_trees();
    println!("Day 3 part 2: {}", part2result);
}
