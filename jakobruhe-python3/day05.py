#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-05
# 06:24 p1 solved
# 06:33 p2 solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return input.strip().split("\n");

def bin_str_to_num(bin_str, one):
    value = 0
    for d in bin_str:
        value = value << 1
        if d == one:
            value = value | 1
    return value

def calculate_seat_id(bsp):
    row = bin_str_to_num(bsp[:7], "B")
    col = bin_str_to_num(bsp[7:], "R")
    return row * 8 + col

def solve1(entries):
    highest = None
    for e in entries:
        seat_id = calculate_seat_id(e)
        if highest is None or seat_id > highest:
            highest = seat_id
    return highest

def solve2(entries):
    seats = []
    for e in entries:
        seats.append(calculate_seat_id(e))
    seats = sorted(seats)
    for i in range(len(seats) - 1):
        if seats[i] + 1 != seats[i+1]:
            return seats[i] + 1

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """
"""
    def test1(self):
        self.assertEqual(calculate_seat_id("FBFBBFFRLR"), 357)
        self.assertEqual(calculate_seat_id("BFFFBBFRRR"), 567)
        self.assertEqual(calculate_seat_id("FFFBBBFRRR"), 119)
        self.assertEqual(calculate_seat_id("BBFFBBFRLL"), 820)
    def test2(self):
        #self.assertEqual(solve2(parse_input(self.input)), 336)
        pass

if __name__ == "__main__":
    with open("input/day05.txt") as f:
        input = f.read()
    entries = parse_input(input)
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
