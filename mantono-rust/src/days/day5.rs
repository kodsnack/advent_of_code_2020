use std::str::Chars;

pub fn first(input: String) -> String {
    input
}

pub fn second(input: String) -> String {
    input
}

fn seat_id(input: &str) -> usize {
    let mut chars = input.chars();
    let row = dbg!(row_num(&mut chars, 0, 127));
    let col = col_num(&mut chars, 0, 7);

    (row * col * 8) as usize
}

fn row_num(iter: &mut Chars<'_>, min: u8, max: u8) -> u8 {
    print!("min {}, max {} ==> {:?} ==> ", min, max, iter);
    if min == max {
        min
    } else {
        let range = (max - min) + 1;
        let next = min + (range / 2);
        match iter.next() {
            Some('F') => row_num(iter, min, next - 1),
            Some('B') => row_num(iter, next, max),
            Some(x) => panic!("Unexpected char: {}", x),
            None => panic!("Out ouf chars"),
        }
    }
}

fn col_num(iter: &mut Chars<'_>, min: u8, max: u8) -> u8 {
    dbg!(&iter);
    if min == max {
        min
    } else {
        match iter.next() {
            Some('L') => row_num(iter, min, max / 2),
            Some('R') => row_num(iter, max / 2, max),
            Some(x) => panic!("Unexpected char: {}", x),
            None => panic!("Out ouf chars"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{row_num, seat_id};

    #[test]
    fn test_seat_id() {
        assert_eq!(567, seat_id("BFFFBBFRRR"));
        assert_eq!(119, seat_id("FFFBBBFRRR"));
        assert_eq!(820, seat_id("BBFFBBFRLL"));
    }

    #[test]
    fn test_row_num() {
        let row = "FBFBBFF";
        let mut row = row.chars();
        assert_eq!(44, row_num(&mut row, 0, 127));
    }
}
