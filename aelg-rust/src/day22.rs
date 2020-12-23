use crate::error;
use std::collections::HashSet;

type Data = (Vec<usize>, Vec<usize>);

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  let mut p1 = Vec::<usize>::new();
  let mut p2 = Vec::<usize>::new();
  let mut p1_done = false;
  for l in input {
    if l == &"Player 1:" {
      continue;
    }
    if l == &"" {
      p1_done = true;
    }
    if let Ok(x) = l.parse() {
      if p1_done {
        p2.push(x)
      } else {
        p1.push(x)
      }
    }
  }
  Ok((p1, p2))
}

fn solve1(v: &Data) -> Option<String> {
  let (mut p1, mut p2) = v.clone();
  while let (Some(x), Some(y)) = (p1.first().copied(), p2.first().copied()) {
    p1 = p1[1..].to_vec();
    p2 = p2[1..].to_vec();
    if x > y {
      p1.push(x);
      p1.push(y);
    } else {
      p2.push(y);
      p2.push(x);
    }
  }
  let score = p1
    .iter()
    .enumerate()
    .map(|(i, x)| x * (p1.len() - i))
    .sum::<usize>()
    + p2
      .iter()
      .enumerate()
      .map(|(i, x)| x * (p2.len() - i))
      .sum::<usize>();
  Some(format!("{:?}", score))
}

fn score(p1: &[usize], p2: &[usize]) -> usize {
  p1.iter()
    .enumerate()
    .map(|(i, x)| x * (p1.len() - i))
    .sum::<usize>()
    + p2
      .iter()
      .enumerate()
      .map(|(i, x)| x * (p2.len() - i))
      .sum::<usize>()
}

fn game((a, b): &(&[usize], &[usize])) -> (bool, usize) {
  let mut p1 = a.to_vec();
  let mut p2 = b.to_vec();
  let mut p1_won = true;
  let mut hands = HashSet::<(Vec<usize>, Vec<usize>)>::new();
  while let (Some(x), Some(y)) = (p1.first().copied(), p2.first().copied()) {
    // How to avoid this clone?
    if hands.contains(&(p1.clone(), p2.clone())) {
      return (true, 0);
    }
    hands.insert((p1.clone(), p2.clone()));
    p1 = p1[1..].to_vec();
    p2 = p2[1..].to_vec();
    if x <= p1.len() && y <= p2.len() {
      p1_won = game(&(&p1[..x], &p2[..y])).0;
    } else if x > y {
      p1_won = true;
    } else {
      p1_won = false;
    }
    if p1_won {
      p1.push(x);
      p1.push(y);
    } else {
      p2.push(y);
      p2.push(x);
    }
  }
  (p1_won, score(&p1, &p2))
}

fn solve2((a, b): &Data) -> Option<String> {
  let (_, score) = game(&(&a, &b));
  Some(format!("{}", score))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("306".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("291".into()))
  }
}
