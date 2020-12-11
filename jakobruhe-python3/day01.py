#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-01
# When visiting the site I get this:
# 503 Service Temporarily Unavailable
# So it indeed looks like adventofcode.com is
# more popular than ever!
# 06:10: I am now able to visit the site.
# 06:23: p1 solved
# 06:27: p2 solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

def solve1(entries):
    for x in entries:
        for y in entries:
            if x + y == 2020:
                return x * y

def solve2(entries):
    for x in entries:
        for y in entries:
            for z in entries:
                if x + y + z == 2020:
                    return x * y * z

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    entries = [1721, 979, 366, 299, 675, 1456]
    def test1(self):
        self.assertEqual(solve1(self.entries), 514579)
    def test2(self):
        self.assertEqual(solve2(self.entries), 241861950)

if __name__ == "__main__":
    with open("input/day01.txt") as f:
        lines = f.readlines()
    entries = [int(line) for line in lines]
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
