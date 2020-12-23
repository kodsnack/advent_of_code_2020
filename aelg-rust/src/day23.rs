use crate::error;

type Data = Vec<usize>;

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  if let [s] = input {
    Ok(
      s.chars()
        .map(|x| x.to_string().parse::<usize>().map(|x| x - 1))
        .flatten()
        .collect(),
    )
  } else {
    Err("parse failed".into())
  }
}

fn solve1(v: &Data) -> Option<String> {
  let mut v = v.clone();
  for _ in 0..100 {
    match &v[..] {
      [cur, a, b, c, rest @ ..] => {
        let mut destination = (cur + 8) % 9;
        while destination == *cur || destination == *a || destination == *b || destination == *c {
          destination = (destination + 8) % 9;
        }
        let mut next: Vec<usize> = Vec::new();
        for x in rest {
          next.push(*x);
          if *x == destination {
            next.push(*a);
            next.push(*b);
            next.push(*c);
          }
        }
        next.push(*cur);
        v = next;
      }
      _ => return None,
    }
  }
  let idx = v.iter().position(|x| *x == 0)?;
  v.rotate_left(idx);
  Some(
    v[1..]
      .iter()
      .map(|x| format!("{}", x + 1))
      .collect::<String>(),
  )
}

fn solve2(v: &Data) -> Option<String> {
  let (ncups, iterations) = (1000000, 10000000);
  let v = v.clone();
  let mut inserts: Vec<usize> = vec![0; ncups];
  let first = *v.first().unwrap();
  inserts.insert(ncups - 1, first);
  let mut cur = first;
  for next in &v[1..] {
    inserts[cur] = *next;
    cur = *next;
  }
  for next in 9..ncups {
    inserts[cur] = next;
    cur = next;
  }
  cur = first;
  for _ in 0..iterations {
    let a = inserts[cur];
    let b = inserts[a];
    let c = inserts[b];
    let next = inserts[c];
    let mut destination = (cur + ncups - 1) % (ncups);
    while destination == a || destination == b || destination == c {
      destination = (destination + ncups - 1) % (ncups);
    }
    let after_destination = inserts[destination];
    inserts[cur] = next;
    inserts[destination] = a;
    inserts[c] = after_destination;
    cur = next;
  }
  let a = *inserts.get(0).unwrap();
  let b = *inserts.get(a).unwrap();
  Some(format!("{}", (a + 1) * (b + 1)))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;
  const EXAMPLE: &str = "389125467";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("67384529".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("149245887792".into()))
  }
}
