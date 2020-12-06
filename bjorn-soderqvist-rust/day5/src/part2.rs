pub fn calculate(s: &str) -> i32 {
    let mut checking = -1;
    let mut numbers: Vec<i32> = s.lines().map(|x| get_seat_id(x)).collect();
    numbers.sort();
    for num in &numbers {
        if checking > -1 && checking != *num {
            return checking;
        }
        checking = num + 1;
    }
    panic!("I can't find any available seats :(");
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
