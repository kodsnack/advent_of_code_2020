use std::str::Chars;

pub fn first(input: String) -> String {
    input
}

pub fn second(input: String) -> String {
    input
}

fn seat_id(input: &str) -> usize {
    let chars = input.chars();
    0
}

fn row_num(iter: &mut Chars<'_>, min: u8, max: u8) -> u8 {
    if min == max {
        min
    } else {
        match iter.next() {
            Some('F') => row_num(iter, min, max / 2),
            Some('B') => row_num(iter, min / 2, max),
            Some(x) => panic!("Unexpected char: {}", x),
            None => panic!("Out ouf chars"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::seat_id;

    #[test]
    fn test_seat_id() {
        assert_eq!(567, seat_id("BFFFBBFRRR"));
        assert_eq!(119, seat_id("FFFBBBFRRR"));
        assert_eq!(820, seat_id("BBFFBBFRLL"));
    }
}
