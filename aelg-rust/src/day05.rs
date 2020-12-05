use crate::error;
//use crate::parser;

type Line = u32;

type Data = Vec<Line>;

pub fn parse<'a>(input: &'a [&str]) -> Result<Data, error::AOCError> {
  fn f(s: &str) -> Result<u32, std::num::ParseIntError> {
    let b = s
      .chars()
      .map(|x| if x == 'B' || x == 'R' { '1' } else { '0' })
      .collect::<String>();
    u32::from_str_radix(b.as_str(), 2)
  }
  input
    .iter()
    .map(|l| f(l).map_err(|_| format!("Couldn't parse {} to int", l).as_str().into()))
    .collect()
}

fn solve1(mut v: Data) -> Option<String> {
  v.sort_unstable();
  v.last().map(|x| format!("{}", x))
}

fn solve2(mut v: Data) -> Option<String> {
  fn calc(v: Data) -> Option<u32> {
    let last = v.last()?;
    let first = v.first()?;
    let sum = v.iter().sum::<u32>();
    let all_sum = ((v.len() as u32 + 1) * (last + first)) / 2;
    println!("{} {} {} {}", first, last, sum, all_sum);
    Some(all_sum - sum)
  }
  v.sort_unstable();
  if let Some(res) = calc(v) {
    return Some(format!("{}", res));
  }
  None
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(v.clone()), solve2(v))
}
