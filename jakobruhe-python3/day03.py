#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-03
# 06:11 started
# 06:29 p1 solved
# 06:36 p2 solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return input.strip().split("\n");

def is_tree_at(entries, x, y):
    width = len(entries[0])
    return entries[y][x % width] == "#"

def count_trees(entries, sx, sy):
    x, y = (0, 0)
    trees = 0
    while True:
        x += sx
        y += sy
        if y >= len(entries):
            break
        if is_tree_at(entries, x, y):
            trees += 1
    return trees

def solve1(entries):
    return count_trees(entries, 3, 1)

def solve2(entries):
    speeds = ((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    trees = 1
    for s in speeds:
        trees *= count_trees(entries, s[0], s[1])
    return trees

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 7)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 336)

if __name__ == "__main__":
    with open("input/day03.txt") as f:
        input = f.read()
    entries = parse_input(input)
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
