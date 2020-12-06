
import math


def get_input():
    with open("input_day5.txt", 'r') as input_file:
        lines = input_file.readlines()
    return lines


def get_row(row):
    upper = 127
    lower = 0
    for c in row:
        if c == 'F':
            upper -= math.ceil((upper - lower) / 2)
        else:
            lower += math.ceil((upper - lower) / 2)
    return upper


def get_col(col):
    upper = 7
    lower = 0
    for c in col:
        if c == 'L':
            upper -= math.ceil((upper - lower) / 2)
        else:
            lower += math.ceil((upper - lower) / 2)
    return upper


def get_seat_id(line):
    return (get_row(line[:- 3]) * 8) + get_col(line[7:])


def solve_a():
    lines = get_input()
    highest = 0
    for line in lines:
        sid = get_seat_id(line)
        highest = sid if sid > highest else highest

    return highest


def solve_b():
    possible_id = [i for i in range(1024)]

    lines = get_input()
    for line in lines:
        possible_id.remove(get_seat_id(line))

    return possible_id


print(solve_a())

print(solve_b())
