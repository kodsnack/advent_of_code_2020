use super::lib;

pub fn solve(data: lib::Flight) -> usize {
    println!("solving Game of Flight");
    let mut flight = tick_flight(&data);
    loop {
        let new_flight = tick_flight(&flight);
        if new_flight == flight {
            return lib::get_all_occupied_seats(&flight);
        }
        flight = new_flight;
    }
}

fn tick_flight(flight: &lib::Flight) -> lib::Flight {
    let layout = flight
        .layout
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(|(x, _pos)| {
                    tick_seat(flight, x, y)
                })
                .collect()
        })
        .collect();
    lib::Flight {
        layout,
        x: flight.x,
        y: flight.y,
    }
}

fn tick_seat(flight: &lib::Flight, x: usize, y: usize) -> char {
    let pos = flight.layout[y][x];
    let occupied_seats = lib::get_adjacent_occupied_seats(flight, x, y);
    // println!("ticking seat? {} {}", pos, occupied_seats);
    if pos == 'L' && occupied_seats == 0 {
        return '#';
    }
    if pos == '#' && occupied_seats >= 4 {
        return 'L';
    }
    pos
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tick_seat() {
        let flight = lib::parse_file(include_str!("tests/layout1.txt"));
        println!("{}", flight);
        assert_eq!(tick_seat(&flight, 1, 1), '#');
        assert_eq!(tick_seat(&flight, 0, 0), '#');
        assert_eq!(tick_seat(&flight, flight.y - 1, flight.x - 1), '#');
    }
    #[test]
    fn test_tick_flight() {
        let flight1 = lib::parse_file(include_str!("tests/layout1.txt"));
        let flight2 = lib::parse_file(include_str!("tests/layout2.txt"));
        let flight3 = lib::parse_file(include_str!("tests/layout3.txt"));
        let flight4 = lib::parse_file(include_str!("tests/layout4.txt"));
        let flight5 = lib::parse_file(include_str!("tests/layout5.txt"));
        let flight6 = lib::parse_file(include_str!("tests/layout6.txt"));
        assert_eq!(tick_flight(&flight1).layout, flight2.layout);
        assert_eq!(tick_flight(&flight2).layout, flight3.layout);
        assert_eq!(tick_flight(&flight3).layout, flight4.layout);
        assert_eq!(tick_flight(&flight4).layout, flight5.layout);
        assert_eq!(tick_flight(&flight5).layout, flight6.layout);
    }
    #[test]
    fn test_solve() {
        let flight = lib::parse_file(include_str!("tests/layout1.txt"));
        assert_eq!(solve(flight), 37);
    }
}
