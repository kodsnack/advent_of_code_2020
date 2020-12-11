#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-07
# 07:12: p1 solved
# 07:18: p2 solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    entries = {}
    for line in input.strip().split("\n"):
        m1 = re.match(r"(\w+ \w+) bags contain no other bags.", line)
        if m1:
            assert(m1.group(1) not in entries)
            entries[m1.group(1)] = {}
            continue
        splitted = line.split(", ")
        m2 = re.match(r"(\w+ \w+) bags contain (\d+) (\w+ \w+) bags?", line)
        color = m2.group(1)
        contains = {m2.group(3): int(m2.group(2))}
        for s in splitted[1:]:
            m = re.match(r"(\d+) (\w+ \w+) bags?", s)
            contains[m.group(2)] = int(m.group(1))
        assert(color not in entries)
        entries[color] = contains
    return entries

def can_contain(entries, current_color, color):
    entry = entries[current_color]
    for col, num in entry.items():
        if col == color:
            return True
        elif can_contain(entries, col, color):
            return True
    return False

def solve1(entries):
    # How many bag colors can contains at least one bag with
    # the color shiny gold?
    count = 0
    for current_color, children in entries.items():
        if can_contain(entries, current_color, "shiny gold"):
            count += 1
    return count

def count_children(entries, color):
    entry = entries[color]
    total = 0
    for col, num in entry.items():
        total += num
        total += num * count_children(entries, col)
    return total

def solve2(entries):
    # How many individual bags are required inside your single shiny gold bag?
    return count_children(entries, "shiny gold")

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"""
    input2 = """vibrant beige bags contain 5 plaid turquoise bags, 2 shiny gold bags, 2 clear tan bags, 1 wavy black bag.
"""
    input3 = """shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 4)
        #self.assertEqual(solve1(parse_input(self.input2)), 1)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 32)
        self.assertEqual(solve2(parse_input(self.input3)), 126)

if __name__ == "__main__":
    with open("input/day07.txt") as f:
        input = f.read()
    entries = parse_input(input)
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
