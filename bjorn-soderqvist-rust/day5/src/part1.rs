pub fn calculate(s: &str) -> i32 {
    let numbers: Vec<i32> = s.lines().map(|x| get_seat_id(x)).collect();
    numbers.iter().cloned().fold(0, i32::max)
}

fn get_seat_id(passport: &str) -> i32 {
    let i_arr: Vec<i32> = passport.chars().map(
        |x| match x {
            'B' | 'R' => 1,
            _ => 0,
        }
    ).collect();
    i_arr.iter().fold(0, |acc, x| acc * 2 + x)
}


#[cfg(test)]
mod tests {
    use super::*;

    static TESTDATA: &str =
"FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL";

    #[test]
    fn test_get_seat_id() {
        assert_eq!(get_seat_id("FBFBBFFRLR"), 357);
        assert_eq!(get_seat_id("BFFFBBFRRR"), 567);
        assert_eq!(get_seat_id("FFFBBBFRRR"), 119);
        assert_eq!(get_seat_id("BBFFBBFRLL"), 820);
    }

    #[test]
    fn test_get_highest_seat_id() {
        assert_eq!(calculate(TESTDATA), 820);
    }
}
