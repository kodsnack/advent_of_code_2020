use crate::error;
use crate::parser::IResult;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Seat {
  Occupied,
  Empty,
  Floor,
}
use Seat::*;

type Row = Vec<Seat>;

type Grid = Vec<Row>;

fn parser(input: &str) -> IResult<&str, Vec<Seat>> {
  use crate::parser::*;
  many1(alt((
    map(char('#'), |_| Occupied),
    (map(char('L'), |_| Empty)),
    (map(char('.'), |_| Floor)),
  )))(input)
}

pub fn parse(input: &[&str]) -> Result<Grid, error::AOCError> {
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

fn seat_update(v: &Grid, vision_len: i64, min_occupied: i64) -> Grid {
  let mut g = v.clone();
  let dirs: Vec<(i64, i64)> = vec![
    (0, 1),
    (0, -1),
    (1, 0),
    (1, 1),
    (1, -1),
    (-1, 0),
    (-1, 1),
    (-1, -1),
  ];
  loop {
    let mut changed = false;
    let mut next = g.clone();
    for (y, r) in next.iter_mut().enumerate() {
      for (x, s) in r.iter_mut().enumerate() {
        let mut occupied = 0;
        if *s == Floor {
          continue;
        }
        for (dx, dy) in &dirs {
          for i in 0..vision_len {
            if let Some(a) = g.get((y as i64 + (i + 1) * dy) as usize) {
              if let Some(b) = a.get((x as i64 + (i + 1) * dx) as usize) {
                if *b == Occupied {
                  occupied += 1;
                  break;
                }
                if *b == Empty {
                  break;
                }
              } else {
                break;
              }
            } else {
              break;
            }
          }
        }
        if *s == Empty && occupied == 0 {
          *s = Occupied;
          changed = true;
        }
        if *s == Occupied && occupied >= min_occupied {
          *s = Empty;
          changed = true;
        }
      }
    }
    g = next;
    if !changed {
      break;
    }
  }
  g
}

fn count_occupied(g: &Grid) -> usize {
  g.iter()
    .fold(0, |a, r| r.iter().filter(|s| **s == Occupied).count() + a)
}

fn solve1(v: &Grid) -> Option<String> {
  let g = seat_update(v, 1, 4);
  Some(format!("{}", count_occupied(&g)))
}

fn solve2(v: &Grid) -> Option<String> {
  let g = seat_update(v, i64::MAX, 5);
  Some(format!("{}", count_occupied(&g)))
}

pub fn solve(v: Grid) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("37".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("26".into()))
  }
}
