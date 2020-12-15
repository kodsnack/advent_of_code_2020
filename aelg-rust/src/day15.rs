use crate::error;

type Data = Vec<u32>;

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  if let [l, ..] = *input {
    return Ok(
      l.split(',')
        .map(|x| x.parse::<u32>())
        .flatten()
        .collect::<Vec<u32>>(),
    );
  }
  Err(format!("failed to parse {:?}", input).as_str().into())
}

fn run(v: &Data, last_turn: u32) -> u32 {
  use std::collections::HashMap;
  let mut m: HashMap<u32, u32> = HashMap::new();
  let mut turn = 0;
  let mut last;
  let mut spoken = 0;
  for x in v {
    last = spoken;
    turn += 1;
    spoken = *x;
    m.insert(last, turn - 1);
  }
  loop {
    if turn == last_turn {
      break;
    }
    last = spoken;
    turn += 1;
    if let Some(x) = m.get(&last) {
      spoken = (turn - 1) - *x;
    } else {
      spoken = 0;
    }
    m.insert(last, turn - 1);
  }
  spoken
}

fn solve1(v: &Data) -> Option<String> {
  Some(format!("{}", run(v, 2020)))
}

fn solve2(v: &Data) -> Option<String> {
  Some(format!("{}", run(v, 30000000)))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "0,3,6";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("436".into()))
  }

  // Takes to long time.
  //#[test]
  //fn test_example2() {
  //  let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
  //  let result = solve2(&parsed);
  //  assert_eq!(result, Some("175594".into()))
  //}
}
