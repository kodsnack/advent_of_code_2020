use crate::error;
use std::collections::{HashMap, HashSet};

type Coord = (i64, i64, i64);
type Coord4 = (i64, i64, i64, i64);

pub fn parse(input: &[&str]) -> Result<HashSet<Coord>, error::AOCError> {
  let mut s = HashSet::<Coord>::new();
  for (y, line) in input.iter().enumerate() {
    for (x, c) in line.chars().enumerate() {
      if c == '#' {
        s.insert((x as i64, y as i64, 1));
      }
    }
  }
  Ok(s)
}

fn step3(s: &HashSet<Coord>) -> HashSet<Coord> {
  let mut ns = HashMap::<Coord, i64>::new();
  let mut res = HashSet::<Coord>::new();
  for (x, y, z) in s {
    for dx in -1..2 {
      for dy in -1..2 {
        for dz in -1..2 {
          if (dx, dy, dz) != (0, 0, 0) {
            let e = ns.entry((x + dx, y + dy, z + dz)).or_default();
            *e += 1;
          }
        }
      }
    }
  }
  for (c, n) in ns {
    if n == 3 || (n == 2 && s.contains(&c)) {
      res.insert(c);
    }
  }
  res
}

fn step4(s: &HashSet<Coord4>) -> HashSet<Coord4> {
  let mut ns = HashMap::<Coord4, i64>::new();
  let mut res = HashSet::<Coord4>::new();
  for (x, y, z, w) in s {
    for dx in -1..2 {
      for dy in -1..2 {
        for dz in -1..2 {
          for dw in -1..2 {
            if (dx, dy, dz, dw) != (0, 0, 0, 0) {
              let e = ns.entry((x + dx, y + dy, z + dz, w + dw)).or_default();
              *e += 1;
            }
          }
        }
      }
    }
  }
  for (c, n) in ns {
    if n == 3 || (n == 2 && s.contains(&c)) {
      res.insert(c);
    }
  }
  res
}

fn solve1(v: &HashSet<Coord>) -> Option<String> {
  let mut s = v.clone();
  for _ in 0..6 {
    s = step3(&s);
  }
  Some(format!("{:?}", s.len()))
}

fn solve2(v: &HashSet<Coord>) -> Option<String> {
  let mut s = HashSet::<Coord4>::new();
  for (x, y, z) in v {
    s.insert((*x, *y, *z, 1));
  }
  for _ in 0..6 {
    s = step4(&s);
  }
  Some(format!("{:?}", s.len()))
}

pub fn solve(v: HashSet<Coord>) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = ".#.
..#
###";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("112".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("848".into()))
  }
}
