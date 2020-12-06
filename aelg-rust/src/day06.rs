use crate::error;

type Data<'a> = Vec<Vec<String>>;

pub fn parse<'a>(input: &'a [&str]) -> Result<Data<'a>, error::AOCError> {
  let a: Vec<String> = input.iter().map(|x| x.to_string()).collect();
  let v: Vec<Vec<String>> = a.split(|x| *x == "").map(Vec::from).collect();
  Ok(v)
}

fn solve1(v: Data) -> Option<String> {
  fn f(x: &Vec<String>) -> i32 {
    let all = x.concat();
    let mut set = std::collections::HashSet::new();
    for c in all.bytes() {
      set.insert(c);
    }
    set.len() as i32
  }
  let sum: i32 = v.iter().map(f).sum();
  Some(format!("{}", sum))
}

fn solve2(v: Data) -> Option<String> {
  fn f(x: &Vec<String>) -> i32 {
    let all = x.concat();
    let num_people = x.len() as i32;
    let mut map = std::collections::HashMap::<u8, i32>::new();
    for c in all.bytes() {
      let v = map.entry(c).or_default();
      *v += 1i32;
    }
    map.values().filter(|x| **x == num_people).count() as i32
  }
  let sum: i32 = v.iter().map(f).sum();
  Some(format!("{}", sum))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(v.clone()), solve2(v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "abc

a
b
c

ab
ac

a
a
a
a

b
";

  #[test]
  fn test_example1() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve1).unwrap();
    assert_eq!(result, Some("11".into()))
  }

  #[test]
  fn test_example2() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve2).unwrap();
    assert_eq!(result, Some("6".into()))
  }
}
