use crate::error;
use crate::parser::{run_parser, IResult};
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

#[derive(Debug)]
pub struct Line {
  ingredients: Vec<String>,
  allergens: Vec<String>,
}

type Data = Vec<Line>;

fn parser(input: &str) -> IResult<&str, Line> {
  use crate::parser::*;
  let ingredients = separated_list1(tag(" "), map(alpha1, |x: &str| x.into()));
  let allergens = delimited(
    tag(" ("),
    preceded(
      tag("contains "),
      separated_list1(tag(", "), map(alpha1, |x: &str| x.into())),
    ),
    tag(")"),
  );
  let (input, (ingredients, allergens)) = tuple((ingredients, allergens))(input)?;
  Ok((
    input,
    Line {
      ingredients,
      allergens,
    },
  ))
}

pub fn parse(input: &[&str]) -> Result<Data, error::AOCError> {
  run_parser(parser, input)
}

fn get_allergen_map(v: &Data) -> HashMap<&str, HashSet<&str>> {
  let mut allergen_map = HashMap::<&str, HashSet<&str>>::new();
  for line in v {
    let ingredients: HashSet<&str> =
      HashSet::from_iter(line.ingredients.iter().map(|x| x.as_str()));
    for allergen in &line.allergens {
      let entry = allergen_map
        .entry(allergen)
        .or_insert_with(|| ingredients.clone());
      *entry = HashSet::from_iter(entry.intersection(&ingredients).copied());
    }
  }
  allergen_map
}

fn solve1(v: &Data) -> Option<String> {
  let allergen_map = get_allergen_map(v);
  let maybe_allergens: HashSet<&str> = HashSet::from_iter(allergen_map.values().flatten().copied());
  let mut ans = 0;
  for line in v {
    for ingredient in &line.ingredients {
      if !maybe_allergens.contains(ingredient.as_str()) {
        ans += 1;
      }
    }
  }
  Some(format!("{}", ans))
}

fn solve2(v: &Data) -> Option<String> {
  let mut allergen_map = get_allergen_map(v);
  let mut canonical_map: HashMap<&str, &str> = HashMap::new();
  while let Some((allergen, ingredient)) = allergen_map.clone().iter().find(|(_, v)| v.len() == 1) {
    let found = ingredient.iter().last()?;
    canonical_map.insert(allergen, found);
    for v in allergen_map.values_mut() {
      v.remove(found);
    }
  }
  let mut canonical_list: Vec<(&str, &str)> = canonical_map.into_iter().collect();
  canonical_list.sort();

  Some(
    canonical_list
      .iter()
      .map(|(_, a)| *a)
      .collect::<Vec<&str>>()
      .join(","),
  )
}

pub fn solve(v: Data) -> (Option<String>, Option<String>) {
  (solve1(&v), solve2(&v))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::testhelper::input_from_str;

  const EXAMPLE: &str = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)";

  #[test]
  fn test_example1() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve1(&parsed);
    assert_eq!(result, Some("5".into()))
  }

  #[test]
  fn test_example2() {
    let parsed = parse(&input_from_str(EXAMPLE)).unwrap();
    let result = solve2(&parsed);
    assert_eq!(result, Some("mxmxvkd,sqjhc,fvjkl".into()))
  }
}
