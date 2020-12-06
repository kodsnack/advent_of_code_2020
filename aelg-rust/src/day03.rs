use crate::error;

type Grid = Vec<Vec<bool>>;

pub fn parse<'a>(input: &'a [&str]) -> Result<Grid, error::AOCError> {
  let mut grid = Grid::new();
  for line in input.iter() {
    let row: Vec<bool> = line.chars().map(|c| c == '#').collect::<Vec<bool>>();
    grid.push(row)
  }
  Ok(grid)
}

fn path_check(right: i64, down: i64, v: &[Vec<bool>]) -> Option<i64> {
  let width = v.get(0).map(Vec::<bool>::len)?;
  let mut pos = 0;
  let mut sum = 0i64;
  for (i, row) in v.iter().enumerate() {
    if i % down as usize != 0 {
      continue;
    }
    let tree = row.get(pos)?;
    if *tree {
      sum += 1;
    }
    pos = (pos + right as usize) % width
  }
  Some(sum)
}

fn solve1(v: Grid) -> Option<String> {
  let sum = path_check(3, 1, &v)?;
  Some(format!("{}", sum))
}

fn solve2(v: Grid) -> Option<String> {
  let paths = vec![
    path_check(1, 1, &v),
    path_check(3, 1, &v),
    path_check(5, 1, &v),
    path_check(7, 1, &v),
    path_check(1, 2, &v),
  ]
  .into_iter()
  .collect::<Option<Vec<i64>>>()?;
  let product: i64 = paths.iter().product();
  Some(format!("{}", product))
}

pub fn solve(v: Grid) -> (Option<String>, Option<String>) {
  (solve1(v.clone()), solve2(v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
";

  #[test]
  fn test_example1() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve1).unwrap();
    assert_eq!(result, Some("7".into()))
  }

  #[test]
  fn test_example2() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve2).unwrap();
    assert_eq!(result, Some("336".into()))
  }
}
