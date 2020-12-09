#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-02
# 06:20 p1 solved
# 06:27 p2 solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

Entry = namedtuple('Entry', ['min', 'max', 'char', 'passwd'])

def parse_input(input):
    entries = []
    for line in input.strip().split("\n"):
        m = re.match(r"(\d+)-(\d+) ([a-z]): ([a-z]+)", line)
        entries.append(Entry(int(m.group(1)), int(m.group(2)), m.group(3), m.group(4)))
    return entries

def is_valid_passwd1(entry):
    num = entry.passwd.count(entry.char)
    return num >= entry.min and num <= entry.max

def solve1(entries):
    return sum(is_valid_passwd1(e) for e in entries)

def is_valid_passwd2(entry):
    v1 = entry.passwd[entry.min - 1] == entry.char
    v2 = entry.passwd[entry.max - 1] == entry.char
    return v1 ^ v2

def solve2(entries):
    return sum(is_valid_passwd2(e) for e in entries)

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 2)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 1)

if __name__ == "__main__":
    with open("input/day02.txt") as f:
        input = f.read()
    entries = parse_input(input)
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
