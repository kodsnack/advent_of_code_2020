use crate::error;
use crate::parser::{run_parser, IResult};

type Data = Vec<String>;

fn parser1(input: &str) -> IResult<&str, i64> {
  use crate::parser::*;

  fn parens(input: &str) -> IResult<&str, i64> {
    delimited(tag("("), expr, tag(")"))(input)
  }
  fn term(input: &str) -> IResult<&str, i64> {
    alt((integer::<i64>, parens))(input)
  }
  fn expr(input: &str) -> IResult<&str, i64> {
    map(
      tuple((term, many1(tuple((alt((tag(" + "), tag(" * "))), term))))),
      |(a, l)| {
        let mut a = a;
        for (op, b) in l {
          if op == " + " {
            a += b;
          } else {
            a *= b;
          }
        }
        a
      },
    )(input)
  }
  expr(input)
}

fn parser2(input: &str) -> IResult<&str, i64> {
  use crate::parser::*;

  let parens = delimited(tag("("), parser2, tag(")"));
  let term = alt((parens, integer::<i64>));
  let sum = map(separated_list1(tag(" + "), term), |l| l.iter().sum::<i64>());
  let mut mult = map(separated_list1(tag(" * "), sum), |l| {
    l.iter().product::<i64>()
  });
  mult(input)
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  Ok(input.iter().map(|x| String::from(*x)).collect())
}

fn solve1(v: &[String]) -> Option<String> {
  match run_parser(
    parser1,
    &v.iter().map(|x| x.as_str()).collect::<Vec<&str>>(),
  ) {
    Ok(l) => Some(format!("{}", l.iter().sum::<i64>())),
    _ => None,
  }
}

fn solve2(v: &[String]) -> Option<String> {
  match run_parser(
    parser2,
    &v.iter().map(|x| x.as_str()).collect::<Vec<&str>>(),
  ) {
    Ok(l) => Some(format!("{}", l.iter().sum::<i64>())),
    Err(e) => {
      println!("{}", e);
      None
    }
  }
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "1 + 2 * 3 + 4 * 5 + 6";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("71".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("231".into()))
  }
}
