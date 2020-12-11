pub fn calculate(code: Vec<usize>) -> usize {
    hack_xmas(&code, 25)
}

fn hack_xmas(n_sequence: &Vec<usize>, preamble: usize) -> usize {
    let n_sequence_len = n_sequence.len();
    let iter_max = n_sequence_len - preamble - 1;
    for i in 0..iter_max {
        let slc_max = i + preamble;
        let slc = n_sequence.get(i..slc_max).unwrap().to_vec();
        let suspect = n_sequence[slc_max];
        let result = try_exploit(&slc, suspect);
        match result {
            Some(_) => (),
            _ => return suspect,
        };
    }
    panic!("This stream is unhackable!!1");
}

fn try_exploit(n_sequence: &Vec<usize>, suspect: usize) -> Option<usize> {
    for n1 in n_sequence {
        for n2 in n_sequence {
            if n1 == n2 {
                continue;
            }
            if n1 + n2 == suspect {
                return Some(suspect);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    static TESTDATA: &str = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576";

    #[test]
    fn test_try_exploit1to25() {
        let data = vec![1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25];
        assert_eq!(try_exploit(&data, 26), Some(26));
        assert_eq!(try_exploit(&data, 49), Some(49));
        assert_eq!(try_exploit(&data, 100), None);
        assert_eq!(try_exploit(&data, 50), None);
    }
    #[test]
    fn test_hack_xmas1to25() {
        let data = vec![1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25];
        assert_eq!(try_exploit(&data, 26), Some(26));
        assert_eq!(try_exploit(&data, 49), Some(49));
        assert_eq!(try_exploit(&data, 100), None);
        assert_eq!(try_exploit(&data, 50), None);
    }
    #[test]
    fn test_try_exploit2to25and45() {
        let data = vec![1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,22,23,24,25,45];
        assert_eq!(try_exploit(&data, 26), Some(26));
        assert_eq!(try_exploit(&data, 65), None);
        assert_eq!(try_exploit(&data, 64), Some(64));
        assert_eq!(try_exploit(&data, 66), Some(66));
    }
    #[test]
    fn test_try_exploit2() {
        let data: Vec<usize> = TESTDATA.lines().map(
            |x| x.parse().unwrap()
        ).collect();
        assert_eq!(try_exploit(&data, 41), None);
        assert_eq!(try_exploit(&data, 40), Some(40));
    }
    #[test]
    fn test_hack_xmas() {
        let data: Vec<usize> = TESTDATA.lines().map(
            |x| x.parse().unwrap()
        ).collect();
        assert_eq!(hack_xmas(&data, 5), 127);
    }
}
