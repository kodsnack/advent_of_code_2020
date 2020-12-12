use crate::error;
use crate::parser::{run_parser, IResult};
use crate::AOCResult;

type Line = (char, i64);

type Data = Vec<Line>;

fn parser(input: &str) -> IResult<&str, Line> {
  use crate::parser::*;
  tuple((anychar, integer))(input)
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  run_parser(parser, input)
}

fn f((x, y, dir): (i64, i64, i64), (a, n): &(char, i64)) -> AOCResult<(i64, i64, i64)> {
  let dirs = [(1, 0), (0, -1), (-1, 0), (0, 1)];
  match a {
    'E' => Ok((x + n, y, dir)),
    'S' => Ok((x, y - n, dir)),
    'W' => Ok((x - n, y, dir)),
    'N' => Ok((x, y + n, dir)),
    'F' => {
      let (dx, dy) = dirs[dir as usize];
      Ok((x + n * dx, y + n * dy, dir))
    }
    'L' => Ok((x, y, (dir - (n / 90) + 4) % 4)),
    'R' => Ok((x, y, (dir + (n / 90)) % 4)),
    _ => Err("invalid op".into()),
  }
}

fn solve1(v: &Data) -> Option<String> {
  match v.iter().try_fold((0, 0, 0), f) {
    Ok((x, y, _)) => Some(format!("{}", x.abs() + y.abs())),
    Err(e) => Some(format!("{}", e)),
  }
}

fn f2(
  (x, y, wx, wy): (i64, i64, i64, i64),
  (a, n): &(char, i64),
) -> crate::AOCResult<(i64, i64, i64, i64)> {
  match a {
    'E' => Ok((x, y, wx + n, wy)),
    'S' => Ok((x, y, wx, wy - n)),
    'W' => Ok((x, y, wx - n, wy)),
    'N' => Ok((x, y, wx, wy + n)),
    'F' => Ok((x + wx * n, y + wy * n, wx, wy)),
    'L' => match n {
      90 => Ok((x, y, -wy, wx)),
      180 => Ok((x, y, -wx, -wy)),
      270 => Ok((x, y, wy, -wx)),
      _ => Err("L with invalid angle".into()),
    },
    'R' => match n {
      90 => Ok((x, y, wy, -wx)),
      180 => Ok((x, y, -wx, -wy)),
      270 => Ok((x, y, -wy, wx)),
      _ => Err("R with invalid angle".into()),
    },
    _ => Err("invalid op".into()),
  }
}

fn solve2(v: &Data) -> Option<String> {
  match v.iter().try_fold((0, 0, 10, 1), f2) {
    Ok((x, y, _, _)) => Some(format!("{}", x.abs() + y.abs())),
    Err(e) => Some(format!("{}", e)),
  }
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "F10
N3
F7
R90
F11";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("25".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("286".into()))
  }
}
