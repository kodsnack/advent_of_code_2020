use crate::error;
use crate::parser::IResult;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct Rule {
  name: String,
  pos: Option<usize>,
  a: i64,
  b: i64,
  c: i64,
  d: i64,
}

type Ticket = Vec<i64>;

#[derive(Debug, Clone)]
pub struct Data {
  rules: Vec<Rule>,
  ticket: Ticket,
  other_tickets: Vec<Ticket>,
}

fn parser(input: &str) -> IResult<&str, Data> {
  use crate::parser::*;
  let limits = map(
    tuple((
      tag(": "),
      integer,
      tag("-"),
      integer,
      tag(" or "),
      integer,
      tag("-"),
      integer,
      space0,
    )),
    |(_, a, _, b, _, c, _, d, _)| (a, b, c, d),
  );
  let rule = map(
    preceded(opt(newline), tuple((take_until(":"), limits))),
    |(name, (a, b, c, d))| Rule {
      name: name.into(),
      pos: None,
      a,
      b,
      c,
      d,
    },
  );
  fn ticket(input: &str) -> IResult<&str, Ticket> {
    terminated(separated_list1(tag(","), integer), opt(newline))(input)
  }
  let (input, rules) = terminated(many1(rule), many1(newline))(input)?;
  let (input, myticket) =
    preceded(tag("your ticket:\n"), terminated(ticket, many1(newline)))(input)?;
  let (input, other_tickets) = preceded(tag("nearby tickets:\n"), many1(ticket))(input)?;
  Ok((
    input,
    Data {
      rules,
      ticket: myticket,
      other_tickets,
    },
  ))
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  let input = input.join("\n");
  match parser(input.as_str()) {
    Ok(("", data)) => Ok(data),
    Ok((a, _)) => Err(format!("Unparsed input {}", a).as_str().into()),
    Err(e) => Err(format!("{}", e).as_str().into()),
  }
}

fn test_fields(rules: &[Rule], tickets: &[Ticket]) -> i64 {
  let mut sum = 0i64;
  for ticket in tickets {
    for v in ticket {
      let mut valid = false;
      for rule in rules {
        if *v >= rule.a && *v <= rule.b {
          valid = true;
        }
        if *v >= rule.c && *v <= rule.d {
          valid = true;
        }
      }
      if !valid {
        sum += *v;
      }
    }
  }
  sum
}

fn solve1(v: &Data) -> Option<String> {
  Some(format!("{:?}", test_fields(&v.rules, &v.other_tickets)))
}

fn valid_tickets(rules: &[Rule], tickets: &[Ticket]) -> Vec<Ticket> {
  let mut res = Vec::<Ticket>::new();
  for ticket in tickets {
    let mut sum = 0i64;
    for v in ticket {
      let mut valid = false;
      for rule in rules {
        if *v >= rule.a && *v <= rule.b {
          valid = true;
        }
        if *v >= rule.c && *v <= rule.d {
          valid = true;
        }
      }
      if !valid {
        sum += *v;
      }
    }
    if sum == 0 {
      res.push(ticket.clone());
    }
  }
  res
}

fn find_rule_pos(mut v: Data, valid: &[Ticket]) -> Data {
  let pos_sum: usize = (21 * 20) / 2;
  let mut complete_fields = HashSet::<usize>::new();
  loop {
    let mut found = false;
    for rule in &mut v.rules {
      let mut bad_fields = complete_fields.clone();
      for ticket in valid {
        for (i, a) in ticket.iter().enumerate() {
          if !((*a >= rule.a && *a <= rule.b) || (*a >= rule.c && *a <= rule.d)) {
            bad_fields.insert(i);
          }
        }
      }
      if bad_fields.len() + 1 == v.ticket.len() {
        let pos = pos_sum - bad_fields.iter().sum::<usize>();
        rule.pos = Some(pos);
        complete_fields.insert(pos);
        found = true;
      }
    }
    if !found {
      break;
    }
  }
  v
}

fn solve2(v: &Data) -> Option<String> {
  let v = v.clone();
  let valid = valid_tickets(&v.rules, &v.other_tickets);
  let v = find_rule_pos(v, &valid);
  let mut result = 1;
  for rule in &v.rules {
    if rule.name.starts_with("departure") {
      match rule.pos {
        Some(pos) => match v.ticket.get(pos) {
          Some(x) => result *= x,
          None => {
            println!("Too few fields on your ticket");
            return None;
          }
        },
        _ => {
          println!("Couldn't find all rule names");
          return None;
        }
      }
    }
  }
  Some(format!("{}", result))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE1: &str = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12";

  const EXAMPLE2: &str = "departure class: 0-1 or 4-19
departure row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE1)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("71".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE2)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("132".into()))
  }
}
