struct Bag {
    color: String,
    contents: Vec<(usize, String)>,
}
impl std::fmt::Display for Bag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Bag color: {}, children: {:?}", self.color, self.contents)
    }
}

pub fn calculate(s: &str) -> usize {
    let parsed_lines = s.lines().map(|l| line_to_bag(l));
    let bags: Vec<Bag> = parsed_lines.clone().collect();
    let result: usize = bag_amount_inside_hack(&bags, "shiny gold".to_string());
    result
}

fn bag_amount_inside(bags: &Vec<Bag>, containing_bag_color: String) -> usize {
    let bag = bags.iter().find(|bag| bag.color == containing_bag_color).unwrap();
    let kidsum: usize = bag.contents.iter().map(
        |bag_inside| {
            let amount = bag_inside.0;
            let inside_color = bag_inside.1.clone();
            if amount == 0 {
                return 1;
            } else {
                let inside = bag_amount_inside(bags, inside_color);
                match inside {
                    0 => amount,
                    _ => amount * inside,
                }
            }
        }
    ).sum();
    kidsum + 1
}

fn bag_amount_inside_hack(bags: &Vec<Bag>, containing_bag_color: String) -> usize {
    bag_amount_inside(bags, containing_bag_color) - 1
}

fn line_to_bag(s: &str) -> Bag {
    let mut iter = s.split(" bags contain ");
    let color = iter.next().unwrap().to_string();
    let s_contents = iter.next().unwrap();
    let contents = match s_contents {
        "no other bags." => vec![],
        _ => s_contents
            .split(", ")
            .map(|c| parse_contents(c))
            .collect(),
    };

    Bag {
        color,
        contents,
    }
}

fn parse_contents(s: &str) -> (usize, String) {
    let number: usize = s[..1].parse().unwrap();
    let rest = &s[2..];
    let result = rest
        .replace(" bags.", "")
        .replace(" bag.", "")
        .replace(" bags", "")
        .replace(" bag", "");
    (number, result)
}


#[cfg(test)]
mod tests {
    use super::*;

    static TESTDATA: &str = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.";

    static TESTDATA2: &str = "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.";

#[test]
    fn test_bag_amount_inside1() {
        let bags:Vec<Bag> = TESTDATA.lines().map(|l| line_to_bag(l)).collect();
        assert_eq!(bag_amount_inside_hack(&bags, "faded blue".to_string()), 0, "faded blue");
        assert_eq!(bag_amount_inside_hack(&bags, "vibrant plum".to_string()), 11, "vibrant plum");
        assert_eq!(bag_amount_inside_hack(&bags, "dark olive".to_string()), 7, "dark olive");
        assert_eq!(bag_amount_inside_hack(&bags, "shiny gold".to_string()), 32, "shiny gold");
    }

#[test]
    fn test_bag_amount_inside2() {
        let bags:Vec<Bag> = TESTDATA2.lines().map(|l| line_to_bag(l)).collect();
        assert_eq!(bag_amount_inside_hack(&bags, "dark violet".to_string()), 0, "dark violet");
        assert_eq!(bag_amount_inside_hack(&bags, "dark blue".to_string()), 2, "dark blue");
        assert_eq!(bag_amount_inside_hack(&bags, "dark green".to_string()), 6, "dark green");
        assert_eq!(bag_amount_inside_hack(&bags, "shiny gold".to_string()), 126, "shiny gold");
    }
}
