#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-17
#
# 06:31 p1 solved
# 06:54 p2 solved

import math
import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return input.strip().split("\n")

def get_neighbor_locations(loc):
    nbs = []
    for z in range(-1, 2):
        for y in range(-1, 2):
            for x in range(-1, 2):
                if len(loc) == 3 and (z != 0 or y != 0 or x != 0):
                    nbs.append((loc[0] + x, loc[1] + y, loc[2] + z))
                elif len(loc) == 4:
                    for w in range(-1, 2):
                        if z != 0 or y != 0 or x != 0 or w != 0:
                            nbs.append((loc[0] + w, loc[1] + x, loc[2] + y, loc[3] + z))
    return nbs

def add_neighbors(loc, num_neighbors):
    cubes = get_neighbor_locations(loc)
    for c in cubes:
        num_neighbors[c] += 1

def simulate_step(active):
    next_active = set()
    num_neighbors = defaultdict(int)
    for c in active:
        add_neighbors(c, num_neighbors)
    for c,num in num_neighbors.items():
        is_active = c in active
        if is_active and (num == 2 or num == 3):
            next_active.add(c)
        elif not is_active and num == 3:
            next_active.add(c)
    return next_active

# How many cubes are left in the active state after the sixth cycle?
def solve(entries, dims):
    active = set()
    for y, row in enumerate(entries):
        for x, c in enumerate(row):
            if c == "#":
                if dims == 3:
                    active.add((x, y, 0))
                elif dims == 4:
                    active.add((0, x, y, 0))
                else:
                    raise ValueError("Dims must be 3 or 4")
    for step in range(6):
        active = simulate_step(active)
    return len(active)

def solve1(entries):
    return solve(entries, 3)

def solve2(entries):
    return solve(entries, 4)

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """.#.
..#
###
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 112)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 848)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print("P1", solve1(entries))
    print("P2", solve2(entries))
