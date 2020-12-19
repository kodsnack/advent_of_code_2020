use crate::error;
use crate::parser::{run_parser, IResult};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
enum Rule {
  Match(char),
  Ref1(usize),
  Ref2(usize, usize),
  Ref3(usize, usize, usize),
  Or1(usize, usize),
  Or2((usize, usize), (usize, usize)),
}
use Rule::*;

#[derive(Debug, Clone)]
pub struct RuleEntry {
  n: usize,
  rule: Rule,
}

type Line = (Option<RuleEntry>, Option<String>);

type Data = (Vec<RuleEntry>, Vec<String>);

fn parser(input: &str) -> IResult<&str, Line> {
  use crate::parser::*;
  fn rule(input: &str) -> IResult<&str, Rule> {
    let or2 = map(
      tuple((
        integer,
        tag(" "),
        integer,
        tag(" | "),
        integer,
        tag(" "),
        integer,
      )),
      |(a, _, b, _, c, _, d)| Or2((a, b), (c, d)),
    );
    let or1 = map(tuple((integer, tag(" | "), integer)), |(a, _, b)| Or1(a, b));
    let match_ = map(delimited(tag("\""), one_of("ab"), tag("\"")), Match);
    let ref1 = map(integer, Ref1);
    let ref2 = map(tuple((integer, tag(" "), integer)), |(a, _, b)| Ref2(a, b));
    let ref3 = map(
      tuple((integer, tag(" "), integer, tag(" "), integer)),
      |(a, _, b, _, c)| Ref3(a, b, c),
    );
    alt((or2, or1, ref3, ref2, ref1, match_))(input)
  }
  let rule_entry = map(tuple((integer, tag(": "), rule)), |(n, _, rule)| {
    RuleEntry { n, rule }
  });
  let (input, (a, _, b)) =
    complete(tuple((opt(rule_entry), opt(newline), opt(is_a("ab")))))(input)?;
  Ok((input, (a, b.map(|b| b.into()))))
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  let r = run_parser(parser, input)?;
  let rules = r.iter().map(|(x, _)| x.clone()).flatten().collect();
  let s = r.iter().map(|(_, x)| x.clone()).flatten().collect();
  Ok((rules, s))
}

fn gen_strings(m: &HashMap<usize, &Rule>, n: &usize) -> Option<Vec<String>> {
  let rule = m.get(n)?;
  match rule {
    Match(c) => Some(vec![String::from(*c)]),
    Ref1(r) => gen_strings(m, r),
    Ref2(r1, r2) => {
      let a = gen_strings(m, r1)?;
      let b = gen_strings(m, r2)?;
      let mut res = Vec::<String>::new();
      for x in a {
        for y in &b {
          res.push(x.clone() + &y);
        }
      }
      Some(res)
    }
    Ref3(r1, r2, r3) => {
      let a = gen_strings(m, r1)?;
      let b = gen_strings(m, r2)?;
      let c = gen_strings(m, r3)?;
      let mut res = Vec::<String>::new();
      for x in a {
        for y in &b {
          for z in &c {
            res.push(x.clone() + &y + &z);
          }
        }
      }
      Some(res)
    }
    Or1(r1, r2) => {
      let mut a = gen_strings(m, r1)?;
      let mut b = gen_strings(m, r2)?;
      a.append(&mut b);
      Some(a)
    }
    Or2((r1, r2), (r3, r4)) => {
      let a = gen_strings(m, r1)?;
      let b = gen_strings(m, r2)?;
      let c = gen_strings(m, r3)?;
      let d = gen_strings(m, r4)?;
      let mut res1 = Vec::<String>::new();
      for x in a {
        for y in &b {
          res1.push(x.clone() + &y);
        }
      }
      let mut res2 = Vec::<String>::new();
      for x in c {
        for y in &d {
          res2.push(x.clone() + &y);
        }
      }
      res1.append(&mut res2);
      Some(res1)
    }
  }
}

fn solve1(v: &Data) -> Option<String> {
  let (a, b) = v;
  let m: HashMap<usize, &Rule> = a.iter().map(|r| (r.n, &r.rule)).collect();
  let all: HashSet<String> = gen_strings(&m, &0).iter().cloned().flatten().collect();
  let mut sum = 0;
  for s in b {
    if all.contains(s) {
      sum += 1;
    }
  }
  Some(format!("{}", sum))
}

fn solve2(v: &Data) -> Option<String> {
  let (a, ss) = v;
  let m: HashMap<usize, &Rule> = a.iter().map(|r| (r.n, &r.rule)).collect();
  let a: HashSet<String> = gen_strings(&m, &42).iter().flatten().cloned().collect();
  let b: HashSet<String> = gen_strings(&m, &31).iter().flatten().cloned().collect();
  let mut sum = 0;
  for s in ss {
    if s.len() % 8 != 0 {
      continue;
    }
    if s.len() < 24 {
      continue;
    }
    for i in 2.. {
      let (xs, ys) = s.split_at(i * 8);
      if ys.is_empty() {
        break;
      }
      if ys.len() / 8 + 1 > xs.len() / 8 {
        continue;
      }
      let mut matches = xs
        .chars()
        .collect::<Vec<char>>()
        .chunks(8)
        .all(|s| a.contains(&s.iter().collect::<String>()));
      matches &= ys
        .chars()
        .collect::<Vec<char>>()
        .chunks(8)
        .all(|s| b.contains(&s.iter().collect::<String>()));
      if matches {
        sum += 1;
        break;
      }
    }
  }
  Some(format!("{}", sum))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("2".into()))
  }
}
