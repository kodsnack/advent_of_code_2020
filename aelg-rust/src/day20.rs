use crate::error;
use crate::parser::IResult;
use std::collections::{HashMap, VecDeque};

#[derive(Debug)]
pub struct Tile {
  n: i64,
  tile: Vec<String>,
}

type Data = Vec<Tile>;

fn tileparser(input: &str) -> IResult<&str, Tile> {
  use crate::parser::*;
  let (input, n) = delimited(tag("Tile "), integer, tag(":\n"))(input)?;
  let (input, tile) = separated_list1(newline, map(is_a(".#"), |x: &str| x.into()))(input)?;
  let (input, _) = many0(newline)(input)?;
  Ok((input, Tile { n, tile }))
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  use crate::parser::*;
  let input = input.join("\n");
  let a = many1(tileparser)(input.as_str());
  match a {
    Ok(("", res)) => Ok(res),
    Ok((input, _)) => {
      println!("leftover after parse: {}", input);
      Err("Parse not all matched".into())
    }
    Err(e) => {
      println!("parse failed with error: {}", e);
      Err("Parse failed".into())
    }
  }
}

fn edges(t: &Tile) -> Vec<String> {
  let top = t.tile.first();
  let bottom = t.tile.last();
  let left = t
    .tile
    .iter()
    .map(|s| s.chars().next())
    .flatten()
    .collect::<String>();
  let right = t
    .tile
    .iter()
    .map(|s| s.chars().last())
    .flatten()
    .collect::<String>();
  vec![top, Some(&right), bottom, Some(&left)]
    .iter()
    .flatten()
    .cloned()
    .cloned()
    .collect::<Vec<String>>()
}

fn solve1(v: &Data) -> Option<String> {
  let mut product = 1;
  for (i, x) in v.iter().enumerate() {
    let mut neighbours = 0;
    for (j, y) in v.iter().enumerate() {
      if i == j {
        continue;
      }
      let xn = edges(x);
      let yn = edges(y);
      for xx in &xn {
        for yy in &yn {
          if xx == yy || xx.chars().rev().collect::<String>() == *yy {
            neighbours += 1;
          }
        }
      }
    }
    if neighbours == 2 {
      product *= x.n;
    }
  }
  Some(format!("{:?}", product))
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TileFixed {
  n: usize,
  tile: [[char; 10]; 10],
  top: Option<usize>,
  right: Option<usize>,
  bottom: Option<usize>,
  left: Option<usize>,
}
impl TileFixed {
  fn rotate(&mut self) {
    let mut n = *self;
    for i in 0..10 {
      for j in 0..10 {
        n.tile[j][9 - i] = self.tile[i][j];
      }
    }
    n.top = self.left;
    n.right = self.top;
    n.bottom = self.right;
    n.left = self.bottom;
    *self = n;
  }
  fn flip(&mut self) {
    let mut n = *self;
    for i in 0..10 {
      for j in 0..10 {
        n.tile[i][9 - j] = self.tile[i][j];
      }
    }
    n.left = self.right;
    n.right = self.left;
    *self = n;
  }
  fn top_edge(&self) -> [char; 10] {
    self.tile[0]
  }
  fn right_edge(&self) -> [char; 10] {
    let mut right = [' '; 10];
    for i in 0..10 {
      right[i] = self.tile[i][9];
    }
    right
  }
  fn bottom_edge(&self) -> [char; 10] {
    self.tile[9]
  }
  fn left_edge(&self) -> [char; 10] {
    let mut left = [' '; 10];
    for i in 0..10 {
      left[i] = self.tile[i][0];
    }
    left
  }
}

type TileMap = HashMap<usize, TileFixed>;

fn to_fixed(v: &Data) -> TileMap {
  let mut res: TileMap = HashMap::new();
  for (i, x) in v.iter().enumerate() {
    let mut tile = TileFixed::default();
    tile.n = x.n as usize;
    for (i, row) in x.tile.iter().enumerate() {
      for (j, elem) in row.chars().enumerate() {
        tile.tile[i][j] = elem;
      }
    }
    for (j, y) in v.iter().enumerate() {
      if i == j {
        continue;
      }
      let yn = edges(y);
      if let [top, right, bottom, left] = &edges(x)[..] {
        for yy in &yn {
          if top == yy || top.chars().rev().collect::<String>() == *yy {
            tile.top = Some(y.n as usize);
          }
          if right == yy || right.chars().rev().collect::<String>() == *yy {
            tile.right = Some(y.n as usize);
          }
          if bottom == yy || bottom.chars().rev().collect::<String>() == *yy {
            tile.bottom = Some(y.n as usize);
          }
          if left == yy || left.chars().rev().collect::<String>() == *yy {
            tile.left = Some(y.n as usize);
          }
        }
      }
    }
    res.insert(tile.n, tile);
  }
  res
}

fn find_corner(m: &mut TileMap) -> Option<&mut TileFixed> {
  for tile in m.values_mut() {
    if vec![tile.top, tile.right, tile.bottom, tile.left]
      .iter()
      .flatten()
      .count()
      == 2
    {
      return Some(tile);
    }
  }
  None
}

fn rotate_until<F>(t: &mut TileFixed, f: F)
where
  F: Fn(&TileFixed) -> bool,
{
  if f(t) {
    return;
  }
  t.rotate();
  if f(t) {
    return;
  }
  t.rotate();
  if f(t) {
    return;
  }
  t.rotate();
  if f(t) {
    return;
  }
  t.flip();
  // hmm, maybe to scary.
  rotate_until(t, f);
}

fn rotate_all(m: &mut TileMap, top_left_corner_n: usize) {
  let mut q = VecDeque::<usize>::new();
  q.push_back(top_left_corner_n);
  while let Some(next) = q.pop_front() {
    let cur = *m.get(&next).unwrap();
    if let Some(right) = cur.right {
      let right = m.get_mut(&right).unwrap();
      rotate_until(right, |t| t.left_edge() == cur.right_edge());
      q.push_back(right.n);
    }
    if let Some(bottom) = cur.bottom {
      let bottom = m.get_mut(&bottom).unwrap();
      rotate_until(bottom, |t| t.top_edge() == cur.bottom_edge());
      q.push_back(bottom.n);
    }
  }
}

fn stitch(m: &TileMap, corner: Option<usize>) -> Vec<Vec<u8>> {
  if let Some(corner) = corner {
    let mut a: [Vec<u8>; 8] = [
      Vec::new(),
      Vec::new(),
      Vec::new(),
      Vec::new(),
      Vec::new(),
      Vec::new(),
      Vec::new(),
      Vec::new(),
    ];
    let cur = m.get(&corner).unwrap();
    let mut next = Some(cur);
    while let Some(tile) = next {
      for (i, e) in a.iter_mut().enumerate() {
        e.append(
          &mut tile.tile[i + 1][1..9]
            .iter()
            .map(|x| *x as u8)
            .collect::<Vec<u8>>(),
        );
      }
      next = tile.right.and_then(|x| m.get(&x));
    }
    let mut rest = stitch(m, cur.bottom);
    let mut result = a.to_vec();
    result.append(&mut rest);
    return result;
  }
  vec![]
}

fn rotate_vec(v: &Vec<Vec<u8>>) -> Vec<Vec<u8>> {
  let mut n = v.clone();
  let n_rows = v.len();
  for (i, row) in n.iter_mut().enumerate() {
    for (j, e) in row.iter_mut().enumerate() {
      *e = *v.get(n_rows - j - 1).unwrap().get(i).unwrap();
    }
  }
  n
}

fn flip_vec(v: &Vec<Vec<u8>>) -> Vec<Vec<u8>> {
  let mut n = v.clone();
  for (i, row) in n.iter_mut().enumerate() {
    let n_cols = row.len();
    for (j, e) in row.iter_mut().enumerate() {
      *e = *v.get(i).unwrap().get(n_cols - 1 - j).unwrap();
    }
  }
  n
}

fn count(v: &Vec<Vec<u8>>, c: char) -> usize {
  let mut sum = 0;
  for row in v {
    for x in row {
      if *x == c as u8 {
        sum += 1;
      }
    }
  }
  sum
}

fn count_monster(v: &Vec<Vec<u8>>) -> usize {
  let n = v.clone().iter().flatten().copied().collect::<Vec<u8>>();
  let n_rows = v.len();
  let monster: Vec<u8> = vec![
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
  ]
  .join(vec![" "; n_rows - 20].join("").as_str())
  .as_bytes()
  .iter()
  .copied()
  .collect();

  let mut found_monsters = 0;
  for i in 0..n.len() {
    let mut is_monster = true;
    for (j, c) in monster.iter().enumerate() {
      if *c == b'#' && *n.get(i + j).unwrap_or(&0) != b'#' {
        is_monster = false;
        break;
      }
    }
    if is_monster {
      found_monsters += 1;
    }
  }
  found_monsters
}

fn find_monsters(mut all_tile: Vec<Vec<u8>>) -> usize {
  for _ in 0..4 {
    all_tile = rotate_vec(&all_tile);
    let monsters = count_monster(&all_tile);
    if monsters > 0 {
      return monsters;
    }
  }
  all_tile = flip_vec(&all_tile);
  for _ in 0..4 {
    all_tile = rotate_vec(&all_tile);
    let monsters = count_monster(&all_tile);
    if monsters > 0 {
      return monsters;
    }
  }
  0
}

fn solve2(v: &Data) -> Option<String> {
  let mut m = to_fixed(v);
  // m has tile with all neighbours set correctly
  // the tiles are no rotaded correctly
  // find a corner to start with
  let mut corner = find_corner(&mut m)?;
  // rotate the corner so it can be placed top left
  rotate_until(&mut corner, |t| t.left == None && t.top == None);
  // BFS from the the top left corner and rotate every left and bottom neighbours so they fit
  let corner_n = corner.n; // Please the borrow checker.
  rotate_all(&mut m, corner_n);
  // Stich everything together to a large tile.
  let all_tile = stitch(&m, Some(corner_n));
  // Count all '#'
  let total_hash = count(&all_tile, '#');
  // Count monsters
  let monsters = find_monsters(all_tile);
  // Each monster is 15 '#'
  Some(format!("{}", total_hash - 15 * monsters))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("20899048083289".into()))
  }
  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("273".into()))
  }
}
