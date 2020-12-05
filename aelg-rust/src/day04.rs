use crate::error;
use crate::parser;

#[derive(Debug, Clone)]
pub struct Passport {
  byr: String,
  iyr: String,
  eyr: String,
  hgt: String,
  hcl: String,
  ecl: String,
  pid: String,
  cid: Option<String>,
}

type Data = Vec<Option<Passport>>;

fn parser(input: &str) -> Result<Data, error::AOCError> {
  use parser::*;
  pub fn field(field_name: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
    move |input: &str| {
      let (input, _) = tag(field_name)(input)?;
      let (input, field_value) = take_till1(|c| c == ' ' || c == '\n')(input)?;
      let (input, _) = one_of(" \n")(input)?;
      Ok((input, field_value))
    }
  }

  fn passport_cid(input: &str) -> IResult<&str, Option<Passport>> {
    let (input, (byr, iyr, eyr, hgt, hcl, ecl, pid, cid)) = permutation((
      field("byr:"),
      field("iyr:"),
      field("eyr:"),
      field("hgt:"),
      field("hcl:"),
      field("ecl:"),
      field("pid:"),
      opt(field("cid:")),
    ))(input)?;
    let (input, _) = tag("\n")(input)?;
    Ok((
      input,
      Some(Passport {
        byr: String::from(byr),
        iyr: String::from(iyr),
        eyr: String::from(eyr),
        hgt: String::from(hgt),
        hcl: String::from(hcl),
        ecl: String::from(ecl),
        pid: String::from(pid),
        cid: cid.map(String::from),
      }),
    ))
  }

  fn invalid_passport(input: &str) -> IResult<&str, Option<Passport>> {
    let (input, _) = take_until("\n\n")(input)?;
    let (input, _) = tag("\n\n")(input)?;
    Ok((input, None))
  }

  fn passport(input: &str) -> IResult<&str, Option<Passport>> {
    let (input, passport) = alt((passport_cid, invalid_passport))(input)?;
    Ok((input, passport))
  }

  match many1(passport)(input) {
    Ok((_, s)) => Ok(s),
    Err(e) => Err(format!("failed: {}", e).as_str().into()),
  }
}

pub fn parse<'a>(input: &'a [&str]) -> Result<String, error::AOCError> {
  let mut s = input.join("\n");
  s.push('\n');
  s.push('\n');
  Ok(s)
}

fn solve1(s: String) -> Option<String> {
  if let Ok(v) = parser(&s) {
    return Some(format!("{}", v.into_iter().filter_map(|x| x).count()));
  }
  None
}

fn valid_passport(p: Passport) -> Option<()> {
  use parser::*;
  match integer::<u32>(p.byr.as_str()) {
    Ok(("", y)) => {
      if y < 1920 || y > 2002 {
        return None;
      }
    }
    _ => return None,
  }

  match integer::<u32>(p.iyr.as_str()) {
    Ok(("", y)) => {
      if y < 2010 || y > 2020 {
        return None;
      }
    }
    _ => return None,
  }

  match integer::<u32>(p.eyr.as_str()) {
    Ok(("", y)) => {
      if y < 2020 || y > 2030 {
        return None;
      }
    }
    _ => return None,
  }

  match tuple((integer::<u32>, alt((tag("cm"), tag("in")))))(p.hgt.as_str()) {
    Ok(("", (h, unit))) => {
      if unit == "cm" && (h < 150 || h > 193) || unit == "in" && (h < 59 || h > 76) {
        return None;
      }
    }
    _ => return None,
  }

  // How to make this compile?
  //match tuple((tag("#"), many_m_n(6, 6, is_a("0123456789abcdef"))))(p.hcl.as_str()) {
  //  Ok(("", _)) => (),
  //  _ => return None,
  //}

  for (i, c) in p.hcl.chars().enumerate() {
    if i == 0 {
      if c != '#' {
        return None;
      }
    } else if !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) {
      return None;
    }
  }

  let ecolors = vec!["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];
  if !ecolors.contains(&p.ecl.as_str()) {
    return None;
  }

  if p.pid.len() != 9 {
    return None;
  }
  for c in p.pid.chars() {
    if !(c >= '0' && c <= '9') {
      return None;
    }
  }

  Some(())
}

fn solve2(s: String) -> Option<String> {
  match parser(&s) {
    Ok(v) => Some(format!(
      "{}",
      v.into_iter()
        .filter_map(|x| x.map(valid_passport))
        .filter_map(|x| x)
        .count()
    )),
    Err(e) => Some(format!("{}", e)),
  }
}

pub fn solve(v: String) -> (Option<String>, Option<String>) {
  (solve1(v.clone()), solve2(v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
";

  #[test]
  fn test_example1() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve1).unwrap();
    assert_eq!(result, Some("2".into()))
  }

  #[test]
  fn test_example2() {
    let result = parse(&input_from_str(EXAMPLE)).map(solve2).unwrap();
    assert_eq!(result, Some("2".into()))
  }
}
