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
    input
}
