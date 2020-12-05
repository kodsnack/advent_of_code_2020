use crate::error;
use crate::parser;

fn parser(input: &str) -> Result<Line, error::AOCError> {
  if let Ok((_input, (min, _, max, _, letter, _, _, s))) = parser::tuple((
    parser::integer,
    parser::char('-'),
    parser::integer,
    parser::space1,
    parser::anychar,
    parser::char(':'),
    parser::space1,
    parser::alpha1,
  ))(input)
  {
    return Ok(Line {
      min,
      max,
      letter,
      s,
    });
  }
  Err("failed".into())
}

#[derive(Debug, Clone)]
pub struct Line<'a> {
  min: usize,
  max: usize,
  letter: char,
  s: &'a str,
}

type Data<'a> = Vec<Line<'a>>;

pub fn parse<'a>(input: &'a [&str]) -> Result<Data<'a>, error::AOCError> {
  input
    .iter()
    .map(|l| parser(l).map_err(|_| format!("Couldn't parse {} to int", l).as_str().into()))
    .collect()
}

fn test_line1(l: &Line) -> bool {
  let filtered: String = l.s.chars().filter(|c| *c == l.letter).collect();
  filtered.len() >= l.min && filtered.len() <= l.max
}

fn solve1(v: Data) -> Option<String> {
  let filtered: Data = v.into_iter().filter(test_line1).collect();
  Some(format!("{}", filtered.len()))
}

fn test_line2(l: &Line) -> bool {
  let min = l.s.chars().nth(l.min - 1);
  let max = l.s.chars().nth(l.max - 1);
  let letter = Some(l.letter);
  !((min == letter) && max == letter) && (min == letter || max == letter)
}

fn solve2(v: Data) -> Option<String> {
  let filtered: Data = v.into_iter().filter(test_line2).collect();
  Some(format!("{}", filtered.len()))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(v.clone()), solve2(v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
";

  #[test]
  fn test_example1() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve1).unwrap();
    assert_eq!(result, Some("2".into()))
  }

  #[test]
  fn test_example2() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve2).unwrap();
    assert_eq!(result, Some("1".into()))
  }
}
