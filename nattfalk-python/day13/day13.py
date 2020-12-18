def part1(input):
    earliest_departure_time = int(input[0])
    buses = [int(b) for b in input[1].split(',') if b != 'x']

    selected_departure = earliest_departure_time + max(buses)
    for b in buses:
        departure_time = (earliest_departure_time // b) * b + b
        if departure_time < selected_departure:
            selected_departure = departure_time
            bus_id = b

    wait_time = selected_departure - earliest_departure_time
    print("Part 1:", (wait_time * bus_id))

def part2(input):
    buses = [(idx, int(b)) for idx, b in enumerate(input[1].split(',')) if b != 'x']
    earliest_match = 0
    step = 1
    for offs, bus_id in buses:
        while True:
            if (earliest_match + offs) % bus_id == 0:
                step *= bus_id
                break
            earliest_match += step
    print("Part 2:", earliest_match)

with open('input.txt') as f:
    lines = [l.strip() for l in f]

part1(lines)
part2(lines)
