extern crate nom;
pub use nom::error::{Error, ParseError};
pub use nom::IResult;
pub use nom::InputIter;
pub use nom::InputLength;
pub use nom::InputTake;
pub use nom::Parser;
pub use nom::UnspecializedInput;

pub use nom::branch::{alt, permutation};

pub use nom::bytes::complete::{is_a, tag, take, take_till1, take_until};

pub use nom::character::complete::{
  alpha1, anychar, char, digit1, one_of, satisfy, space0, space1,
};
pub use nom::character::is_hex_digit;

pub use nom::combinator::{complete, map, map_parser, map_res, opt, recognize};

pub use nom::multi::{many0, many1, many_m_n, many_till, separated_list1};

pub use nom::sequence::{pair, preceded, terminated, tuple};

fn decimal(input: &str) -> IResult<&str, &str> {
  recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

pub fn integer<T: std::str::FromStr>(input: &str) -> IResult<&str, T> {
  map_res(decimal, |s| s.parse::<T>())(input)
}
