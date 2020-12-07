use std::{collections::HashMap, sync::Arc};

use regex::Regex;

/// Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!
///
/// For example, consider the following rules:
///
/// ```
/// light red bags contain 1 bright white bag, 2 muted yellow bags.
/// dark orange bags contain 3 bright white bags, 4 muted yellow bags.
/// bright white bags contain 1 shiny gold bag.
/// muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
/// shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
/// dark olive bags contain 3 faded blue bags, 4 dotted black bags.
/// vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
/// faded blue bags contain no other bags.
/// dotted black bags contain no other bags.
/// ```
///
/// These rules specify the required contents for 9 bag types. In this example, every faded blue bag
/// is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.
///
/// You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different
/// bag colors would be valid for the outermost bag?
/// (In other words: how many colors can, eventually, contain at least one shiny gold bag?)
///
/// In the above rules, the following options would be available to you:
///
/// ```
///     A bright white bag, which can hold your shiny gold bag directly.
///     A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
///     A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
///     A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
/// ```
///
/// So, in this example, the number of bag colors that can eventually contain
/// at least one shiny gold bag is 4.
///
/// How many bag colors can eventually contain at least one shiny gold bag?
pub fn first(input: String) -> String {
    let bags = input
        .lines()
        .map(|line: &str| map_bags(line))
        .collect::<HashMap<String, Vec<BagContent>>>();

    count(bags).to_string()
}

fn count(bags: HashMap<String, Vec<BagContent>>) -> usize {
    bags.keys()
        .filter(|bag| can_hold_golden_bag(bag, &bags))
        .count()
        - 1
}

fn can_hold_golden_bag(bag: &str, bags: &HashMap<String, Vec<BagContent>>) -> bool {
    if bag == "shiny gold" {
        return true;
    };
    match bags.get(bag) {
        None => false,
        Some(content) => content
            .iter()
            .map(|content: &BagContent| &content.bag_type)
            .any(|content: &String| can_hold_golden_bag(content, bags)),
    }
}

pub fn second(input: String) -> String {
    let bags = input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line: &str| map_bags(line))
        .collect::<HashMap<String, Vec<BagContent>>>();

    (rec_explore(&bags, "shiny gold")).to_string()
}

// shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
// dark olive bags contain 3 faded blue bags, 4 dotted black bags.
// vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
// faded blue bags contain no other bags.
// dotted black bags contain no other bags.
fn rec_explore(bags: &HashMap<String, Vec<BagContent>>, bag: &str) -> usize {
    let next: &Vec<BagContent> = match bags.get(bag) {
        None => {
            println!("{} return => {}", bag, 1);
            return 1;
        }
        Some(next) => next,
    };

    if next.is_empty() {
        println!("{} return => {}", bag, 1);
        return 1;
    }

    let sum: usize = next
        .iter()
        .map(|c| rec_explore(bags, &c.bag_type) * c.size())
        .sum();

    let ret = sum + 1;
    println!("{} return => {}", bag, ret);
    ret
}

lazy_static::lazy_static! {
    static ref SPLIT: Regex = Regex::new("( bags contain | bags?[,.] ?)").unwrap();
}

fn map_bags(line: &str) -> (String, Vec<BagContent>) {
    let mut parts = SPLIT.split(line);
    let bag_type: String = parts.next().unwrap().to_string();

    let content: Vec<BagContent> = parts
        .filter(|bag| bag != &"no other")
        .filter_map(|bag: &str| BagContent::from(bag))
        .collect::<Vec<BagContent>>();

    (bag_type, content)
}

#[derive(Debug)]
struct BagContent {
    pub quantity: u8,
    pub bag_type: String,
}

impl BagContent {
    pub fn from(input: &str) -> Option<BagContent> {
        let mut chars = input.chars();
        let quantity: u8 = match chars.next() {
            None => return None,
            Some(c) => c.to_digit(10).unwrap() as u8,
        };
        let bag_type: String = chars.skip(1).collect::<String>();
        Some(BagContent { quantity, bag_type })
    }

    fn size(&self) -> usize {
        self.quantity as usize
    }
}

#[cfg(test)]
mod tests {
    use super::{first, second};

    #[test]
    fn test_part1() {
        let input: &str = r"
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
";

        assert_eq!("4", &first(input.to_string()));
    }

    #[test]
    fn test_part2() {
        let input: &str = r"
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
";
        assert_eq!("126", &second(input.to_string()));
    }

    #[test]
    fn test_part2b() {
        let input: &str = r"
shiny gold bags contain 2 dark red bags.
dark red bags contain 1 dark orange bag.
dark orange bags contain no other bags.
";
        assert_eq!("4", &second(input.to_string()));
    }
}
