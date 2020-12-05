pub fn first(input: String) -> String {
    transform(&input).max().unwrap().to_string()
}

pub fn second(input: String) -> String {
    let mut boarding_passes: Vec<u16> = transform(&input).collect::<Vec<u16>>();
    boarding_passes.sort();

    let (prior_seat, _) = boarding_passes
        .iter()
        .zip(boarding_passes.iter().skip(1))
        .find(|(b0, b1)| *b1 - *b0 > 1)
        .unwrap();

    (prior_seat + 1).to_string()
}

fn transform(input: &str) -> impl Iterator<Item = u16> + '_ {
    input.lines().map(|line| seat_id(line))
}

fn seat_id(input: &str) -> u16 {
    row_num(input) * 8 + col_num(input)
}

fn row_num(input: &str) -> u16 {
    let binary: String = input[0..=6].replace("F", "0").replace("B", "1");
    u16::from_str_radix(&binary, 2).unwrap()
}

fn col_num(input: &str) -> u16 {
    let binary: String = input[7..=9].replace("L", "0").replace("R", "1");
    u16::from_str_radix(&binary, 2).unwrap()
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
        assert_eq!(44, row_num(row));
    }
}
