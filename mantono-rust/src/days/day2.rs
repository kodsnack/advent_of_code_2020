use regex::Regex;

/// To try to debug the problem, they have created a list (your puzzle input) of passwords
/// (according to the corrupted database) and the corporate policy when that password was set.
///
/// For example, suppose you have the following list:
///
/// 1-3 a: abcde
/// 1-3 b: cdefg
/// 2-9 c: ccccccccc
///
/// Each line gives the password policy and then the password.
/// The password policy indicates the lowest and highest number of times a given letter must appear
/// for the password to be valid. For example, 1-3 a means that the password must contain a at
/// least 1 time and at most 3 times.
///
/// In the above example, 2 passwords are valid. The middle password, cdefg, is not;
/// it contains no instances of b, but needs at least 1.
/// The first and third passwords are valid: they contain one a or nine c,
/// both within the limits of their respective policies.
///
/// How many passwords are valid according to their policies?
pub fn first(input: String) -> String {
    transform(input)
        .iter()
        .filter(|p| p.is_valid())
        .count()
        .to_string()
}

fn transform(input: String) -> Vec<Password> {
    input.lines().map(|p| Password::from(p)).collect()
}

/// Each policy actually describes two positions in the password, where 1 means the first character,
///  2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no
///  concept of "index zero"!) Exactly one of these positions must contain the given letter.
/// Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
///
/// Given the same example list from above:
///
/// 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
/// 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
/// 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
///
/// How many passwords are valid according to the new interpretation of the policies?
pub fn second(input: String) -> String {
    transform(input)
        .iter()
        .filter(|p| p.is_valid2())
        .count()
        .to_string()
}

#[derive(Debug)]
struct Password {
    pub lower_bound: u8,
    pub upper_bound: u8,
    pub char_required: char,
    pub content: String,
}

lazy_static::lazy_static! {
    static ref REGEX: Regex = Regex::new(r"(\d+|\w+)").unwrap();
}

fn next_str<'a>(iter: &'a mut regex::Matches) -> &'a str {
    iter.next().unwrap().as_str()
}

impl Password {
    fn from(input: &str) -> Password {
        let mut input: regex::Matches = REGEX.find_iter(input);
        let lower_bound: u8 = next_str(&mut input).parse::<u8>().unwrap();
        let upper_bound: u8 = next_str(&mut input).parse::<u8>().unwrap();
        let char_required: char = next_str(&mut input).chars().nth(0).unwrap();
        let content: String = next_str(&mut input).to_string();

        Password {
            lower_bound,
            upper_bound,
            char_required,
            content,
        }
    }

    fn is_valid(&self) -> bool {
        let occurences: u8 = self
            .content
            .chars()
            .filter(|c| *c == self.char_required)
            .count() as u8;

        occurences >= self.lower_bound && occurences <= self.upper_bound
    }

    fn is_valid2(&self) -> bool {
        let first = self
            .content
            .chars()
            .nth(self.lower_bound as usize - 1)
            .unwrap_or('-');
        let second = self
            .content
            .chars()
            .nth(self.upper_bound as usize - 1)
            .unwrap_or('-');
        let first_match = first == self.char_required;
        let second_match = second == self.char_required;
        first_match ^ second_match
    }
}

#[cfg(test)]
mod tests {
    use crate::days::day2::Password;
    #[test]
    fn test_validator2() {
        assert!(Password::from("1-3 a: abcde").is_valid2());
        assert!(!Password::from("1-3 b: cdefg").is_valid2());
        assert!(!Password::from("2-9 c: ccccccccc").is_valid2());
    }
}
