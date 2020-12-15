use crate::error;
use crate::parser::IResult;

#[derive(Debug)]
pub struct Data {
  start: i64,
  ids: Vec<i64>,
}

fn parser(input: &str) -> IResult<&str, Vec<i64>> {
  use crate::parser::*;
  separated_list1(tag(","), alt((map(tag("x"), |_| -1i64), integer)))(input)
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  if let [a, b, ..] = input {
    if let Ok(start) = str::parse(a) {
      if let Ok(("", ids)) = parser(b) {
        return Ok(Data { start, ids });
      }
    }
  }
  Err("couldn't parse".into())
}

fn solve1(v: &Data) -> Option<String> {
  let l = v.ids.iter().filter(|x| **x != -1);
  let (mut id, mut min) = (-1i64, i64::MAX);
  for a in l {
    let next = a - (v.start % a);
    if next < min {
      id = *a;
      min = next;
    }
  }
  Some(format!("{}", id * min))
}

fn f(mut x: i64, n: i64, l: &[(i64, i64)]) -> i64 {
  if let [(a, b), ..] = l {
    loop {
      if (x + *a) % *b == 0 {
        return f(x, n * b, &l[1..]);
      }
      x += n
    }
  }
  x
}

fn solve2(v: &Data) -> Option<String> {
  let l: Vec<(i64, i64)> = v
    .ids
    .iter()
    .enumerate()
    .filter(|(_, x)| **x != -1)
    .map(|(i, x)| (i as i64, *x))
    .collect();
  Some(format!("{}", f(0, 1, &l)))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "939
7,13,x,x,59,x,31,19
";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("295".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("1068781".into()))
  }
}
