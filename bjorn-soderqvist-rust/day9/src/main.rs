mod part1;
mod part2;

fn main() {
    let part1result = part1::calculate(
        include_str!("input.txt").lines().map(
            |x| x.parse().unwrap()
        ).collect()
    );
    println!("Day 9 part 1: {}", part1result);
    let part2result = part2::calculate(
        include_str!("input.txt").lines().map(
            |x| x.parse().unwrap()
        ).collect()
    );
    println!("Day 9 part 2: {}", part2result);
}
