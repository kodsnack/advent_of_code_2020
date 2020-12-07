use lazy_static::lazy_static;
use regex::Regex;

pub fn first(input: String) -> String {
    let regex = Regex::new(r"(byr:|iyr:|eyr:|hgt:|hcl:|ecl:|pid:)").unwrap();
    transform(&input)
        .map(|p| regex.find_iter(&p).count())
        .filter(|p| *p == 7)
        .count()
        .to_string()
}

pub fn second(input: String) -> String {
    transform(&input)
        .filter(|line| is_valid_pass(line))
        .count()
        .to_string()
}

lazy_static! {
    static ref SPLIT: Regex = Regex::new(r"\n\s+").unwrap();
    static ref PASS: Regex =
        Regex::new(r"(byr:(19[2-9]\d|200([0-2]))|iyr:20(1[0-9]|20)|eyr:20(2[0-9]|30)|hgt:((59|6\d|7[0-6])in|1([5-8]\d|9[1-3])cm)|hcl:#[\da-f]{6}|ecl:(amb|blu|brn|gry|grn|hzl|oth)|pid:\d{9})").unwrap();
}

fn transform<'r, 't>(input: &'t String) -> regex::Split<'r, 't> {
    SPLIT.split(input)
}

#[inline(always)]
fn is_valid_pass(input: &str) -> bool {
    PASS.find_iter(input).count() == 7
}

#[cfg(test)]
mod tests {
    use super::first;

    #[test]
    fn test_part1() {
        let passports: &str = r"
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
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

        assert_eq!("2", first(passports.to_string()))
    }
}
