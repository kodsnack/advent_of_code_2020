use crate::error;

type Data = (i64, i64);

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  if let [a, b] = &input[..] {
    if let (Ok(a), Ok(b)) = (a.parse(), b.parse()) {
      return Ok((a, b));
    }
  }
  Err("parse error".into())
}

fn update(x: i64, subject: i64) -> i64 {
  (x * subject) % 20201227
}

fn solve1(v: &Data) -> Option<String> {
  let (a, b) = *v;
  let mut aloop = 0;
  let mut p = 1;
  for i in 1.. {
    p = update(p, 7);
    if p == a {
      aloop = i;
      break;
    }
  }
  p = 1;
  for _ in 0..aloop {
    p = update(p, b);
  }

  Some(format!("{:?}", p))
}

fn solve2(_: &Data) -> Option<String> {
  Some("God jul!".into())
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "5764801
17807724";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("14897079".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, None)
  }
}
