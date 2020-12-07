use std::collections::HashMap;

pub fn calculate(s: &str) -> usize {
    let total = s
        .split("\n\n")
        .map(|x| get_all_in_group(x))
        .sum();
    total
}

fn get_all_in_group(s: &str) -> usize {
    let mut hashmap: HashMap<char, usize> = HashMap::new();
    for (index, line) in s.lines().enumerate() {
        line.chars().for_each(
            |c| *hashmap.entry(c).or_insert(0) += 1
        )
    }
    let n_surveys = s.lines().count();
    let vals = hashmap.values().filter(|x| x == &&n_surveys);
    vals.count()
}

#[cfg(test)]
mod tests {
    use super::*;

    static TESTDATA: &str = "abc

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
    fn test_calculate() {
        assert_eq!(calculate(TESTDATA), 6);
    }
}