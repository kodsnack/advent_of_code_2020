use crate::error;
use crate::parser::IResult;

type Line = i64;

type Data = Vec<Line>;

fn parser(input: &str) -> IResult<&str, Line> {
  use crate::parser::*;
  integer(input)
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
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

fn find_non_match(preamble: usize, v: &Vec<i64>) -> i64 {
  for (i, w) in v.windows(preamble).enumerate() {
    if let Some(n) = v.get(i + preamble) {
      let mut ok = false;
      for a in w {
        for b in w {
          if a != b && a + b == *n {
            ok = true;
          }
        }
      }
      if !ok {
        return *n;
      }
    }
  }
  -1
}

fn find_contig(x: i64, v: &Data) -> i64 {
  for window_size in 2.. {
    for w in v.windows(window_size) {
      if w.iter().sum::<i64>() == x {
        if let (Some(a), Some(b)) = (w.iter().min(), w.iter().max()) {
          return a + b;
        }
      }
    }
  }
  -1
}

fn solve1(v: &Data) -> Option<String> {
  let res = find_non_match(25, v);
  Some(format!("{}", res))
}

fn solve2(v: &Data) -> Option<String> {
  let x = find_non_match(25, v);
  let res = find_contig(x, v);
  Some(format!("{}", res))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = find_non_match(5, &parsed);
    assert_eq!(result, 127);
  }

  #[test]
  fn test_example2() {
    let v = parse(&input_from_str(EXAMPLE)).unwrap();
    let x = find_non_match(5, &v);
    let result = find_contig(x, &v);
    assert_eq!(result, 62)
  }
}
