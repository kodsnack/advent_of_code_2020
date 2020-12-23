use crate::error;
use crate::parser::IResult;

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
  vec![top, bottom, Some(&left), Some(&right)]
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

fn solve2(_v: &Data) -> Option<String> {
  None
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}
