use crate::error;
use crate::parser::IResult;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct ContainRule {
  number: u64,
  color: String,
}

#[derive(Debug, Clone)]
pub struct BagRule {
  color: String,
  contains: Vec<ContainRule>,
}

type Data<'a> = Vec<BagRule>;

fn parser(input: &str) -> IResult<&str, BagRule> {
  use crate::parser::*;
  fn color(input: &str) -> IResult<&str, String> {
    use crate::parser::*;
    map(tuple((alpha1, space1, alpha1)), |(a, _, b)| {
      format!("{} {}", String::from(a), String::from(b))
    })(input)
  }
  let rule = map(
    tuple((
      integer::<u64>,
      space1,
      color,
      alt((tag(" bags"), tag(" bag"))),
    )),
    |(number, _, color, _)| ContainRule { number, color },
  );
  let contain_rules = alt((
    separated_list1(tag(", "), rule),
    map(tag("no other bags"), |_| Vec::new()),
  ));
  map(
    tuple((color, tag(" bags contain "), contain_rules, tag("."))),
    |(color, _, contains, _)| BagRule { color, contains },
  )(input)
}

pub fn parse<'a>(input: &'a [&str]) -> Result<Data<'a>, error::AOCError> {
  input
    .iter()
    .map(|l| {
      parser(l).map_err(|e| {
        format!("Couldn't parse {}\nerror: {}", l, e)
          .as_str()
          .into()
      })
    })
    .map(|r| r.map(|(_, x)| x))
    .collect()
}

fn add_color(
  color: &str,
  rules: &HashMap<String, HashSet<String>>,
  contained: &mut HashSet<String>,
) {
  contained.insert(color.into());
  if let Some(e) = rules.get(color) {
    for a in e.iter() {
      if !contained.contains(a) {
        add_color(a, &rules, contained)
      }
    }
  };
}

fn solve1(v: Data) -> Option<String> {
  let mut m = HashMap::<String, HashSet<String>>::new();
  for rule in v {
    for contained in rule.contains {
      let e = m.entry(contained.color).or_default();
      (*e).insert(rule.color.clone());
    }
  }
  let mut result = HashSet::<String>::new();
  let my_briefcase = String::from("shiny gold");
  add_color(&my_briefcase, &m, &mut result);
  Some(format!("{}", result.len() - 1)) // -1 to ignore shiny gold
}

fn lookup(color: &str, m: &HashMap<String, Vec<ContainRule>>) -> u64 {
  if let Some(case) = m.get(color) {
    let mut sum = 1u64;
    for e in case {
      sum += e.number * lookup(e.color.as_str(), m);
    }
    return sum;
  }
  0
}

fn solve2(v: Data) -> Option<String> {
  let mut m = HashMap::<String, Vec<ContainRule>>::new();
  for rule in v {
    m.insert(rule.color, rule.contains);
  }
  Some(format!("{}", lookup("shiny gold", &m) - 1)) // -1 to ignore shiny gold
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(v.clone()), solve2(v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE1: &str = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.";

  const EXAMPLE2: &str = "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.";

  #[test]
  fn test_example1() {
    let result = parse(&input_from_str(EXAMPLE1)).map(solve1).unwrap();
    assert_eq!(result, Some("4".into()))
  }

  #[test]
  fn test_example2() {
    let result = parse(&input_from_str(EXAMPLE2)).map(solve2).unwrap();
    assert_eq!(result, Some("126".into()))
  }
}
