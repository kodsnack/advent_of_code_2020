use crate::error;
use crate::parser::{run_parser, IResult};
use std::collections::{HashMap, HashSet};

type Coord = (i64, i64, i64);

type Data = Vec<Coord>;

fn add_coord((a, b, c): &Coord, (x, y, z): &Coord) -> Coord {
  (a + x, b + y, c + z)
}

fn parser(input: &str) -> IResult<&str, Coord> {
  use crate::parser::*;
  let dir = alt((
    map(tag("se"), |_| (0, -1, 1)),
    map(tag("sw"), |_| (-1, 0, 1)),
    map(tag("ne"), |_| (1, 0, -1)),
    map(tag("nw"), |_| (0, 1, -1)),
    map(tag("e"), |_| (1, -1, 0)),
    map(tag("w"), |_| (-1, 1, 0)),
  ));
  map(many1(dir), |l| {
    l.iter().fold((0, 0, 0), |x, dx| add_coord(&x, dx))
  })(input)
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  run_parser(parser, input)
}

fn solve1(v: &Data) -> Option<String> {
  let mut m = HashMap::<Coord, bool>::new();
  for c in v {
    let entry = m.entry(*c).or_default();
    *entry = !*entry;
  }
  let nblack = m.values().filter(|x| **x).count();
  Some(format!("{}", nblack))
}

fn solve2(v: &Data) -> Option<String> {
  let mut m = HashMap::<Coord, bool>::new();
  let dirs = vec![
    (0, -1, 1),
    (-1, 0, 1),
    (1, 0, -1),
    (0, 1, -1),
    (1, -1, 0),
    (-1, 1, 0),
  ];
  for c in v {
    let entry = m.entry(*c).or_default();
    *entry = !*entry;
  }
  let mut black_tiles = m
    .iter()
    .filter(|(_, x)| **x)
    .map(|(k, _)| *k)
    .collect::<HashSet<Coord>>();
  for _ in 0..100 {
    let mut neighbours = HashMap::<Coord, i64>::new();
    for tile in &black_tiles {
      for dir in &dirs {
        let entry = neighbours.entry(add_coord(tile, &dir)).or_default();
        *entry += 1;
      }
    }
    black_tiles = neighbours
      .iter()
      .filter_map(|(k, v)| {
        if black_tiles.contains(k) {
          if *v == 0 || *v > 2 {
            None
          } else {
            Some(*k)
          }
        } else if *v == 2 {
          Some(*k)
        } else {
          None
        }
      })
      .collect();
  }
  let nblack = black_tiles.len();
  Some(format!("{}", nblack))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("10".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("2208".into()))
  }
}
