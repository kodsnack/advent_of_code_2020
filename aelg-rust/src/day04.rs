use crate::error;
use crate::parser;
use crate::parser::IResult;

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

type Data = Vec<Passport>;

fn parser(input: &str) -> IResult<&str, Vec<Option<Passport>>> {
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
  let invalid_passport = map(many_till(anychar, tag("\n\n")), |(_, _)| None);

  let passport = alt((passport_cid, invalid_passport));

  many1(passport)(input)
}

pub fn parse<'a>(input: &'a [&str]) -> Result<Data, error::AOCError> {
  let mut s = input.join("\n");
  s.push('\n');
  s.push('\n');

  match parser(s.as_str()) {
    Ok(("", data)) => Ok(data.into_iter().filter_map(|x| x).collect()),
    Ok((input, _)) => Err(
      format!("Couldn't parse, stopped near: {}", input)
        .as_str()
        .into(),
    ),
    Err(e) => Err(
      format!("Couldn't parse {}\nerror: {}", s, e)
        .as_str()
        .into(),
    ),
  }
}

fn solve1(s: &[Passport]) -> Option<String> {
  Some(format!("{}", s.len()))
}

fn valid_passport(p: &&Passport) -> bool {
  use parser::*;
  match integer::<u32>(p.byr.as_str()) {
    Ok(("", y)) => {
      if y < 1920 || y > 2002 {
        return false;
      }
    }
    _ => return false,
  }

  match integer::<u32>(p.iyr.as_str()) {
    Ok(("", y)) => {
      if y < 2010 || y > 2020 {
        return false;
      }
    }
    _ => return false,
  }

  match integer::<u32>(p.eyr.as_str()) {
    Ok(("", y)) => {
      if y < 2020 || y > 2030 {
        return false;
      }
    }
    _ => return false,
  }

  match tuple((integer::<u32>, alt((tag("cm"), tag("in")))))(p.hgt.as_str()) {
    Ok(("", (h, unit))) => {
      if unit == "cm" && (h < 150 || h > 193) || unit == "in" && (h < 59 || h > 76) {
        return false;
      }
    }
    _ => return false,
  }

  match tuple::<_, _, (), _>((tag("#"), many_m_n(6, 6, one_of("0123456789abcdef"))))(p.hcl.as_str())
  {
    Ok(("", _)) => (),
    _ => return false,
  }

  match alt::<_, _, (), _>((
    tag("amb"),
    tag("blu"),
    tag("brn"),
    tag("gry"),
    tag("grn"),
    tag("hzl"),
    tag("oth"),
  ))(p.ecl.as_str())
  {
    Ok(("", _)) => (),
    _ => return false,
  }

  match many_m_n::<_, _, (), _>(9, 9, one_of("0123456789"))(p.pid.as_str()) {
    Ok(("", _)) => (),
    _ => return false,
  }
  true
}

fn solve2(s: &[Passport]) -> Option<String> {
  Some(format!("{}", s.iter().filter(valid_passport).count()))
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
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
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("2".into()))
  }
  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("2".into()))
  }
}
