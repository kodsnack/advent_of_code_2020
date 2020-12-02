pub fn first(input: String) -> String {
    let expenses: Vec<u32> = transform(input);
    compute_first(expenses).to_string()
}

fn transform(input: String) -> Vec<u32> {
    input
        .split_whitespace()
        .map(|n| n.parse::<u32>().unwrap())
        .collect::<Vec<u32>>()
}

fn compute_first(mut report: Vec<u32>) -> u32 {
    report.sort();
    let last_index: usize = report.len() - 1;
    for x in 0..=last_index {
        for y in 0..=last_index {
            let e0: u32 = *report.get(x).unwrap();
            let e1: u32 = *report.get(last_index - y).unwrap();
            if e0 + e1 == 2020 {
                return e0 * e1;
            }
        }
    }
    panic!("Error if we get here")
}

pub fn second(input: String) -> String {
    let expenses: Vec<u32> = transform(input);
    compute_second(expenses).to_string()
}

fn compute_second(mut report: Vec<u32>) -> u32 {
    report.sort();
    let last_index: usize = report.len() - 1;
    for x in 0..=last_index {
        for y in 0..=last_index {
            for z in 0..=last_index {
                let e0: u32 = *report.get(x).unwrap();
                let e1: u32 = *report.get(last_index - y).unwrap();
                let e2: u32 = *report.get(z).unwrap();
                if e0 + e1 + e2 == 2020 {
                    return e0 * e1 * e2;
                }
            }
        }
    }
    panic!("Error if we get here")
}

#[cfg(test)]
mod tests {
    use crate::days::day1::compute_first;
    #[test]
    fn test_example_input() {
        let expense_report: Vec<u32> = vec![1721, 979, 366, 299, 675, 1456];
        let n: u32 = compute_first(expense_report);
        assert_eq!(514_579, n);
    }
}
