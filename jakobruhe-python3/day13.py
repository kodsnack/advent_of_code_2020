#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-13

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple
from math import gcd

def parse_input(input):
    return input.strip().split("\n")

def solve1(entries):
    # What is the ID of the earliest bus you can take to the airport multiplied
    # by the number of minutes you'll need to wait for that bus?
    t = int(entries[0])
    buses = entries[1].split(",")
    nearest = None
    nearest_bus = None
    for b in buses:
        if b == 'x':
            continue
        bus_id = int(b)
        next_departure = (t // bus_id) * bus_id
        if next_departure < t:
            next_departure += bus_id
        departs_in = next_departure - t
        if not nearest or departs_in < nearest:
            nearest_bus = bus_id
            nearest = departs_in
    return nearest_bus * nearest

def solve_with_step(bus, t0, step):
    bus_id, offset = bus
    t = t0
    while True:
        if (t + offset) % bus_id == 0:
            return t
        t += step

def solve2(line):
    # What is the earliest timestamp such that all of the listed bus IDs depart at
    # offsets matching their positions in the list?
    entries = line.split(",")
    buses = []
    for t,bus_id in enumerate(entries):
        if bus_id != 'x':
            buses.append((int(bus_id), t))
    step, t0 = buses[0]
    for b in buses[1:]:
        t = solve_with_step(b, t0, step)
        # Note: This works as long as the bus id:s are prime numbers, which they
        # were in my case and I guess for others as well.
        step *= b[0]
        t0 = t
    return t


# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """939
7,13,x,x,59,x,31,19
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 295)
    def test2(self):
        self.assertEqual(solve2("7,13"), 77)
        self.assertEqual(solve2("7,13,x,x,59"), 350)
        self.assertEqual(solve2("7,13,x,x,59,x,31"), 70147)
        self.assertEqual(solve2("7,13,x,x,59,x,31,19"), 1068781)
        self.assertEqual(solve2("17,x,13,19"), 3417)
        self.assertEqual(solve2("67,7,59,61"), 754018)
        self.assertEqual(solve2("67,x,7,59,61"), 779210)
        self.assertEqual(solve2("67,7,x,59,61"), 1261476)
        self.assertEqual(solve2("1789,37,47,1889"), 1202161486)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(f"{solve1(entries)}")
    print(f"{solve2(entries[1])}")
