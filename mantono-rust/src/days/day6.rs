use std::collections::HashMap;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref GROUPS: Regex = Regex::new(r"\s+\n").unwrap();
}

pub fn first(input: String) -> String {
    GROUPS
        .split(&input)
        .map(sum_group)
        .sum::<usize>()
        .to_string()
}

fn sum_group(group: &str) -> usize {
    let mut answers: Vec<char> = group
        .lines()
        .map(|line| line.trim().chars())
        .flatten()
        .filter(|c: &char| !c.is_ascii_whitespace())
        .collect();

    answers.sort();
    answers.dedup();
    answers.len()
}

pub fn second(input: String) -> String {
    GROUPS
        .split(&input)
        .map(join_group)
        .sum::<usize>()
        .to_string()
}

fn join_group(group: &str) -> usize {
    let answers: Vec<&str> = group.lines().collect::<Vec<&str>>();
    let group_size: usize = answers.len();
    let mut occurences: HashMap<char, usize> = HashMap::with_capacity(16);

    answers
        .iter()
        .map(|line| line.trim().chars())
        .flatten()
        .filter(|c: &char| !c.is_ascii_whitespace())
        .for_each(|c: char| {
            let new: usize = *occurences.get(&c).unwrap_or(&0) + 1;
            occurences.insert(c, new);
        });

    occurences.iter().filter(|(_, v)| **v == group_size).count()
}

#[cfg(test)]
mod tests {
    use super::first;
    #[test]
    fn test_part1() {
        let input = r"
        abc

        a
        b
        c

        ab
        ac

        a
        a
        a
        a

        b
        ";

        assert_eq!("11", &first(input.to_string()));
    }
}
