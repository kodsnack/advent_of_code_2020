use crate::error;
use crate::parser::{run_parser, IResult};
use std::collections::HashMap;

#[derive(Debug)]
pub enum Instr {
  Mask(u64, u64),
  Write(usize, u64),
}

type Data = Vec<Instr>;

fn parser(input: &str) -> IResult<&str, Instr> {
  use crate::parser::*;
  fn mask_from_str(s: &str) -> Result<Instr, std::num::ParseIntError> {
    let zeroes = s
      .chars()
      .map(|x| if x == '0' { '1' } else { '0' })
      .collect::<String>();
    let ones = s
      .chars()
      .map(|x| if x == '1' { '1' } else { '0' })
      .collect::<String>();
    let zeroes = u64::from_str_radix(zeroes.as_str(), 2)?;
    let ones = u64::from_str_radix(ones.as_str(), 2)?;
    Ok(Instr::Mask(zeroes, ones))
  }
  let mask = map_res(preceded(tag("mask = "), is_a("01X")), mask_from_str);
  let mem = map(
    tuple((
      preceded(tag("mem["), terminated(integer, tag("] = "))),
      integer,
    )),
    |(a, b)| Instr::Write(a, b),
  );
  alt((mask, mem))(input)
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  run_parser(parser, input)
}

fn solve1(v: &Data) -> Option<String> {
  let mut mem: HashMap<usize, u64> = HashMap::new();
  let mut zeroes = 0u64;
  let mut ones = 0u64;
  for instr in v {
    match instr {
      Instr::Mask(mzeroes, mones) => {
        zeroes = *mzeroes;
        ones = *mones;
      }
      Instr::Write(pos, value) => {
        let entry = mem.entry(*pos).or_default();
        *entry = value | ones;
        *entry &= !zeroes;
      }
    }
  }
  let sum: u64 = mem.iter().map(|(k, v)| v).sum();
  Some(format!("{}", sum))
}

fn solve2(v: &Data) -> Option<String> {
  let mut mem: HashMap<u64, u64> = HashMap::new();
  let mut zeroes = 0u64;
  let mut ones = 0u64;
  for instr in v {
    match instr {
      Instr::Mask(mzeroes, mones) => {
        zeroes = 0xfffffff000000000 | *mzeroes;
        ones = *mones;
      }
      Instr::Write(pos, value) => {
        // Please don't look at this...
        let pos = *pos as u64;
        let floating = !(zeroes | ones);
        let num_floating = floating.count_ones() as u64;
        for i in 0..(1 << (num_floating)) {
          let (mut zeroes, mut ones) = (0u64, ones);
          let mut floating = floating;
          for j in 0u64..(num_floating) {
            zeroes |= ((!i) & (1 << j)) << (floating.trailing_zeros() as u64 - j);
            ones |= (i & (1 << j)) << (floating.trailing_zeros() as u64 - j);
            floating &= !0u64 << (floating.trailing_zeros() as u64 + 1);
          }
          let masked_pos = (pos | ones) & !zeroes;
          let entry = mem.entry(masked_pos).or_default();
          *entry = *value;
        }
      }
    }
  }
  let sum: u64 = mem.iter().map(|(k, v)| v).sum();
  Some(format!("{}", sum))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
";

  const EXAMPLE2: &str = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("165".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE2)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("208".into()))
  }
}
