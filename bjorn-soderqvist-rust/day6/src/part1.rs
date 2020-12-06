use std::collections::HashSet;

pub fn calculate(s: &str) -> usize {
    let total = s
        .split("\n\n")
        .map(|x| get_unique_group(x))
        .sum();
    total
}

fn get_unique_group(s: &str) -> usize {
    let mut answers: HashSet<char> = HashSet::new();
    s
        .split("\n")
        .for_each(
            |x| x.chars().for_each(
                |c| {
                    answers.insert(c);
                }
            )
        );
    answers.len()
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
        assert_eq!(calculate(TESTDATA), 11);
    }

}