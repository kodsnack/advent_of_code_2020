#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-15

# Note that solve1 and solve2 solves exactly the same problem but solve2
# is optimized. The solve1 was used for part 1 and is kept as it is.

import math
import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return [int(e) for e in input.strip().split(",")]

def solve1(start_numbers, turns):
    history = []
    for n in start_numbers:
        history.insert(0, n)
    for turn in range(len(start_numbers) + 1, turns + 1):
        last = history[0]
        try:
            spoken = history.index(last, 1)
        except ValueError:
            spoken = 0
        history.insert(0, spoken)
    return history[0]

def solve2(start_numbers, turns):
    spoken_numbers = {}
    for i in range(len(start_numbers) - 1):
        spoken_numbers[start_numbers[i]] = i + 1
    last_number_spoken = start_numbers[-1]
    for turn in range(len(start_numbers) + 1, turns + 1):
        turn_last_number_was_spoken = spoken_numbers.get(last_number_spoken, 0)
        spoken_numbers[last_number_spoken] = turn - 1
        if turn_last_number_was_spoken == 0:
            last_number_spoken = 0
        else:
            last_number_spoken = turn - 1 - turn_last_number_was_spoken
    return last_number_spoken

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    def test1(self):
        self.assertEqual(solve1(parse_input("0,3,6"), 2020), 436)
        self.assertEqual(solve1(parse_input("1,3,2"), 2020), 1)
        self.assertEqual(solve1(parse_input("2,1,3"), 2020), 10)
        self.assertEqual(solve1(parse_input("1,2,3"), 2020), 27)
    def test2(self):
        self.assertEqual(solve2(parse_input("0,3,6"), 2020), 436)
        self.assertEqual(solve2(parse_input("1,3,2"), 2020), 1)
        self.assertEqual(solve2(parse_input("0,3,6"), 30000000), 175594)
        self.assertEqual(solve2(parse_input("1,3,2"), 30000000), 2578)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(solve1(entries, 2020))
    print(solve2(entries, 30000000))
