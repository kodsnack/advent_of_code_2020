use crate::error;
use crate::parser::IResult;
use std::collections::HashMap;

type Data = Vec<i64>;

fn parser(input: &str) -> IResult<&str, i64> {
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

fn solve1(v: &Data) -> Option<String> {
  let mut v = v.clone();
  v.sort_unstable();
  let (ones, threes, _) = v
    .iter()
    .fold((0i64, 0i64, 0i64), |(ones, threes, cur), next| {
      match next - cur {
        1 => (ones + 1i64, threes, *next),
        3 => (ones, threes + 1i64, *next),
        _ => (ones, threes, *next),
      }
    });
  Some(format!("{}", ones * (threes + 1)))
}

fn f(v: &[i64], pos: usize, lookup: &mut HashMap<usize, i64>) -> i64 {
  if let [] = v[pos..] {
    return 1;
  }
  if let Some(res) = lookup.get(&pos) {
    return *res;
  }
  let mut s = 0;
  if let [cur, _, next, ..] = v[pos..] {
    if next - cur <= 3 {
      s += f(&v, pos + 2, lookup);
    }
  }
  if let [cur, _, _, next, ..] = v[pos..] {
    if next - cur <= 3 {
      s += f(v, pos + 3, lookup);
    }
  }
  let res = s + f(v, pos + 1, lookup);
  lookup.insert(pos, res);
  res
}

fn solve2(v: &Data) -> Option<String> {
  let mut v = v.clone();
  v.sort_unstable();
  v.insert(0, 0);
  let mut lookup = HashMap::new();
  Some(format!("{}", f(&v, 0, &mut lookup)))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("220".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("19208".into()))
  }
}
