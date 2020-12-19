use std::num::ParseIntError;

pub fn first(input: String) -> String {
    let mut lines = input.lines();
    let timestamp: usize = lines.next().unwrap().parse::<usize>().unwrap();
    let line = lines.next().unwrap();
    let bus_lines: Vec<usize> = line
        .split(",")
        .filter_map(|d| d.parse::<usize>().ok())
        .collect::<Vec<usize>>();

    let mut bus_line_id: usize = 0;
    let mut wait_time: usize = std::usize::MAX;

    for bus in bus_lines {
        let next_due_in = bus - (timestamp % bus);
        if next_due_in < wait_time {
            wait_time = next_due_in;
            bus_line_id = bus;
        }
    }
    (bus_line_id * wait_time).to_string()
}

pub fn second(input: String) -> String {
    let mut bus_lines: Vec<BusLine> = input
        .lines()
        .find(|line| line.contains(","))
        .unwrap()
        .split(",")
        .enumerate()
        .filter_map(|(index, id)| BusLine::from(index, id).ok())
        .collect::<Vec<BusLine>>();

    let last_bus: &BusLine = bus_lines.iter().max_by(|b0, b1| b0.id.cmp(&b1.id)).unwrap();
    let max_id: u64 = last_bus.id;
    let mut timestamp: u64 = 0;
    let mut inc: u64 = 1;
    let mut size: usize = 2;

    loop {
        let lines = &bus_lines[0..size];
        timestamp = next_timestamp(lines, timestamp, inc);
        inc = lines.iter().map(|b| b.id).product();

        if lines.len() == bus_lines.len() {
            break;
        }

        size += 1;
    }

    timestamp.to_string()
}

fn next_timestamp(busses: &[BusLine], timestamp: u64, increment: u64) -> u64 {
    if busses.iter().all(|b| b.time_fits(timestamp)) {
        timestamp
    } else {
        next_timestamp(busses, timestamp + increment, increment)
    }
}

#[derive(Debug)]
struct BusLine {
    pub id: u64,
    pub slot: u64,
}

impl BusLine {
    pub fn from(index: usize, id: &str) -> Result<BusLine, ParseIntError> {
        let id: u64 = id.parse::<u64>()?;
        Ok(BusLine {
            id,
            slot: index as u64,
        })
    }

    pub fn departs_at(&self, timestamp: u64) -> bool {
        let offset = timestamp % self.id;
        offset == 0
    }

    pub fn time_fits(&self, timestamp: u64) -> bool {
        self.departs_at(timestamp + self.slot)
    }
}

#[cfg(test)]
mod tests {
    use super::{second, BusLine};

    #[test]
    fn test_part2_1() {
        assert_eq!("1068781", second("7,13,x,x,59,x,31,19".to_string()));
    }

    #[test]
    fn test_part2_2() {
        assert_eq!("3417", second("17,x,13,19".to_string()));
    }

    #[test]
    fn test_part2_3() {
        assert_eq!("1202161486", second("1789,37,47,1889".to_string()));
    }

    #[test]
    fn test_departs_at() {
        assert!(BusLine::from(0, "17").unwrap().departs_at(3417));
        assert!(BusLine::from(2, "13").unwrap().departs_at(3419));
        assert!(BusLine::from(3, "19").unwrap().departs_at(3420));
    }
}
