pub fn first(input: String) -> String {
    let mut adapters: Vec<usize> = input
        .lines()
        .filter_map(|line| line.parse::<usize>().ok())
        .collect::<Vec<usize>>();

    adapters.sort();

    let mut d1 = 0;
    let mut d3 = 0;

    let mut others: Vec<usize> = adapters.clone();
    let device: usize = *others.last().unwrap() + 3;
    adapters.insert(0, 0);
    others.push(device);
    adapters
        .iter()
        .zip(others)
        .inspect(|(a0, a1)| println!("{} - {}", a1, a0))
        .map(|(a0, a1)| dbg!(a1 - *a0))
        .for_each(|n| match n {
            1 => d1 += 1,
            3 => d3 += 1,
            _ => panic!("NO"),
        });

    (d1 * d3).to_string()
}

pub fn second(input: String) -> String {
    let mut adapters: Vec<usize> = input
        .lines()
        .filter_map(|line| line.parse::<usize>().ok())
        .collect::<Vec<usize>>();

    adapters.sort();
    adapters.insert(0, 0);
    adapters.push(adapters.last().unwrap() + 3);

    count_options(&adapters, 0).to_string()
}

fn count_options(adapters: &[usize], index: usize) -> usize {
    println!("adapters: {:?}", adapters);
    if adapters.is_empty() {
        return 1;
    }
    //std::thread::sleep(std::time::Duration::from_millis(10));

    // Get current adapter + the 3 following ones
    let end = 4.min(adapters.len());
    let mut sub_set = adapters[..end].iter();
    println!("subset: {:?}", sub_set);
    let current: usize = match sub_set.next() {
        None => return 1,
        Some(n) => *n,
    };

    sub_set
        .enumerate()
        .filter(|(i, ad)| **ad - current <= 3)
        .map(|(i, ad)| {
            let range = dbg!(1..);
            let slice = dbg!(&adapters[range]);
            count_options(slice, i) + 1
        })
        .sum()
}

/* fn count_options(adapters: &[usize], current: usize, index: usize) -> usize {
    if adapters.is_empty() {
        return 1;
    }
    //std::thread::sleep(std::time::Duration::from_millis(10));

    adapters
        .iter()
        .take(3)
        .enumerate()
        .inspect(|(i, next)| println!("n: {}", next))
        .filter(|(_, next)| **next - current <= 3)
        .map(|(i, next)| count_options(&adapters[i + 1..], *next, i) + 1)
        .sum()
} */

#[cfg(test)]
mod tests {
    use super::second;

    #[test]
    fn test_part2() {
        let input = r"
16
10
15
5
1
11
7
19
6
12
4";
        // Input:    (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
        // Options:   1   1  3  2  1  1   2   1   1   1   1   1
        // Delta:         1  3  1  1  1   3   1   1   3   1   3   3

        // Input:    (0), 1, [4, 5, 6, 7], [10, 11, 12], 15, 16, 19, (22)
        assert_eq!("8", &second(input.to_string()));
    }
}

// [0, 1, 2, 3, 8]
// --------------
// [0, ]
