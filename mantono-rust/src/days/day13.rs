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

    let mut i: usize = 0;

    loop {
        i += 1;
        if i % 1_000_000_000 == 0 {
            println!();
            for b in &bus_lines {
                println!("{} => {}", b.id, b.abs_timestamp());
            }
        }

        if are_aligned(&bus_lines) {
            break;
        }

        &bus_lines.iter_mut().min().unwrap().next();
    }

    bus_lines.iter().min().unwrap().abs_timestamp().to_string()
}

#[derive(Debug)]
struct BusLine {
    pub id: u64,
    pub slot: u64,
    dep: u64,
}

impl BusLine {
    pub fn from(index: usize, id: &str) -> Result<BusLine, ParseIntError> {
        let id: u64 = id.parse::<u64>()?;
        Ok(BusLine {
            id,
            slot: index as u64,
            dep: 1,
        })
    }

    pub fn abs_timestamp(&self) -> u64 {
        self.id * self.dep
    }

    #[inline]
    pub fn adj_timestamp(&self) -> u64 {
        self.id * self.dep - self.slot
    }
}

impl Iterator for BusLine {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        self.dep += 1;
        Some(self.adj_timestamp())
    }
}

impl Ord for BusLine {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.abs_timestamp() < other.abs_timestamp() {
            std::cmp::Ordering::Less
        } else if self.abs_timestamp() > other.abs_timestamp() {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    }
}

impl PartialOrd for BusLine {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for BusLine {}

impl PartialEq for BusLine {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[inline(always)]
fn are_aligned(busses: &Vec<BusLine>) -> bool {
    let n: u64 = busses.get(0).unwrap().adj_timestamp();
    busses.iter().all(|bus| bus.adj_timestamp() == n)
}

#[cfg(test)]
mod tests {
    use super::second;

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
}
