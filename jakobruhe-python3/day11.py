#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-11

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

FLOOR = "."
EMPTY = "L"
OCCUPIED = "#"
OUTSIDE = " "

DIRECTIONS = ((-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1))

def parse_input(input):
    return input.strip().split("\n")

def get_seat(seats, x, y):
    width = len(seats[0])
    height = len(seats)
    if x < 0 or x >= width or y < 0 or y >= height:
        return " "
    return seats[y][x]

def get_adjacent_seats(seats, x, y):
    return [get_seat(seats, x + d[0], y + d[1]) for d in DIRECTIONS]

def num_adjacent_occupied_seats(seats, x, y):
    return get_adjacent_seats(seats, x, y).count(OCCUPIED)

def num_occupied_seats(seats):
    total = 0
    for row in seats:
        total += row.count(OCCUPIED)
    return total

# If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
# If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
def solve1(seats):
    width = len(seats[0])
    height = len(seats)
    n = 1
    while True:
        next_round = []
        any_change = False
        for y in range(height):
            line_out = []
            for x in range(width):
                seat = get_seat(seats, x, y)
                new_seat = seat
                if seat == EMPTY and num_adjacent_occupied_seats(seats, x, y) == 0:
                    new_seat = OCCUPIED
                elif seat == OCCUPIED and num_adjacent_occupied_seats(seats, x, y) >= 4:
                    new_seat = EMPTY
                any_change = any_change or new_seat != seat
                line_out.append(new_seat)
            next_round.append("".join(line_out))
        seats = next_round[:]
        if not any_change:
            break
        n += 1
    return num_occupied_seats(seats)

def get_visible_seat_in_direction(seats, x, y, direction):
    while True:
        x += direction[0]
        y += direction[1]
        seat = get_seat(seats, x, y)
        if seat != FLOOR:
            return seat

def num_visible_occupied_seats(seats, x, y):
    visible = [get_visible_seat_in_direction(seats, x, y, d) for d in DIRECTIONS]
    return sum(s == OCCUPIED for s in visible)

def solve2(seats):
    width = len(seats[0])
    height = len(seats)
    n = 1
    while True:
        next_round = []
        any_change = False
        for y in range(height):
            line_out = []
            for x in range(width):
                seat = get_seat(seats, x, y)
                new_seat = seat
                if seat == EMPTY and num_visible_occupied_seats(seats, x, y) == 0:
                    new_seat = OCCUPIED
                elif seat == OCCUPIED and num_visible_occupied_seats(seats, x, y) >= 5:
                    new_seat = EMPTY
                any_change = any_change or new_seat != seat
                line_out.append(new_seat)
            next_round.append("".join(line_out))
        seats = next_round[:]
        if not any_change:
            break
        n += 1
    return num_occupied_seats(seats)

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 37)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 26)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
