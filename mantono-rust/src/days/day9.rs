use std::ops::{Range, RangeInclusive};

pub fn first(input: String) -> String {
    let num: Vec<u64> = input
        .lines()
        .filter_map(|n| n.parse::<u64>().ok())
        .collect::<Vec<u64>>();

    let n: &u64 = num
        .iter()
        .enumerate()
        .skip(25)
        .find(|(i, n): &(usize, &u64)| {
            let from = i - 25;
            let to = i - 1;
            !is_valid(**n, &num[from..=to])
        })
        .map(|(_, n)| n)
        .unwrap();

    n.to_string()
}

fn is_valid(n: u64, num: &[u64]) -> bool {
    let num: Vec<u64> = num
        .iter()
        .map(|x| *x)
        .filter(|x| *x < n)
        .collect::<Vec<u64>>();

    for x in &num {
        for y in &num {
            if x + y == n && x != y {
                return true;
            }
        }
    }
    false
}

const TARGET: u64 = 1639024365;

pub fn second(input: String) -> String {
    let num: Vec<u64> = input
        .lines()
        .filter_map(|n| n.parse::<u64>().ok())
        .collect::<Vec<u64>>();

    let mut start: usize = 0;
    let mut end: usize = 2;

    loop {
        let range: Range<usize> = dbg!(start..end);
        let length = range.len();
        let sum: u64 = num[range].iter().sum();
        if sum == TARGET {
            break;
        } else if sum < TARGET {
            end += 1;
        } else {
            start += 1;
            if length < 2 {
                end += 1
            }
        }
    }

    let mut seq: Vec<&u64> = num[start..end].iter().collect::<Vec<&u64>>();
    seq.sort();
    let sum: u64 = **seq.first().unwrap() + **seq.last().unwrap();

    sum.to_string()
}
