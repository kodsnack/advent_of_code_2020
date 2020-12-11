struct Bag {
    color: String,
    contents: Vec<(usize, String)>,
}

pub fn calculate(s: &str) -> usize {
    let parsed_lines = s.lines().map(|l| line_to_bag(l));
    let bags: Vec<Bag> = parsed_lines.clone().collect();
    let result: usize = bags.iter()
        .map(|bag| does_bag_contain(&bags, bag.color.clone(), "shiny gold".to_string()))
        .map(|b| match b {
            true => 1,
            false => 0,
        }).sum();
    result
}

fn does_bag_contain(bags: &Vec<Bag>, container_bag_color: String, containing_bag_color: String) -> bool {
    let bag = bags.iter().find(|bag| bag.color == container_bag_color).unwrap();
    if bag.contents.len() == 0 {
        return false;
    }
    bag.contents.iter().map(
        |bag_inside| {
            let inside_color = bag_inside.1.clone();
            if inside_color == containing_bag_color {
                return true;
            } else {
                return does_bag_contain(bags, inside_color, containing_bag_color.clone());
            }
        }
    ).any(|x| x)
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

    static CONTENT1: &str = "1 bright white bag";
    static CONTENT2: &str = "2 muted yellow bags.";

    #[test]
    fn test_parse_contents() {
        assert_eq!(parse_contents(CONTENT1), (1, "bright white".to_string()));
        assert_eq!(parse_contents(CONTENT2), (2, "muted yellow".to_string()));
    }
    #[test]
    fn test_line_to_bag() {
        let line1 = TESTDATA.lines().next().unwrap();
        let testdata = line_to_bag(line1);
        let expected = Bag {
            color: "light red".to_string(),
            contents: vec![
                (1, "bright white".to_string()),
                (2, "muted yellow".to_string()),
            ],
        };
        assert_eq!(testdata.color, expected.color);
        assert_eq!(testdata.contents[0], expected.contents[0]);
        assert_eq!(testdata.contents[1], expected.contents[1]);
    }
    #[test]
    fn test_does_bag_contain() {
        let bags:Vec<Bag> = TESTDATA.lines().map(|l| line_to_bag(l)).collect();
        // TRUE
        assert_eq!(does_bag_contain(&bags, "bright white".to_string(), "shiny gold".to_string()), true, "bright white");
        assert_eq!(does_bag_contain(&bags, "muted yellow".to_string(), "shiny gold".to_string()), true, "muted yellow");
        assert_eq!(does_bag_contain(&bags, "dark orange".to_string(), "shiny gold".to_string()), true, "dark orange");
        assert_eq!(does_bag_contain(&bags, "light red".to_string(), "shiny gold".to_string()), true, "light red");
        // FALSE
        assert_eq!(does_bag_contain(&bags, "dark olive".to_string(), "shiny gold".to_string()), false, "dark olive");
        assert_eq!(does_bag_contain(&bags, "vibrant plum".to_string(), "shiny gold".to_string()), false, "vibrant plum");
        assert_eq!(does_bag_contain(&bags, "shiny gold".to_string(), "shiny gold".to_string()), false, "shiny gold");
        assert_eq!(does_bag_contain(&bags, "faded blue".to_string(), "shiny gold".to_string()), false, "faded blue");
    }
}
