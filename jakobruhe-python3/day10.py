#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-10

import unittest
import re
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return list(map(int, input.strip().split("\n")))

def solve1(entries):
    # What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
    adapters = sorted(entries)
    diff_map = defaultdict(lambda: 0)
    output = 0
    for a in adapters:
        diff = a - output
        if diff > 3:
            raise ValueError("Diff > 3")
        diff_map[diff] += 1
        output = a
    diff_map[3] += 1
    return diff_map[1] * diff_map[3]

def combinations(device, output, adapters, memory):
    if adapters in memory:
        return memory[adapters]
    if not adapters:
        return output + 3 >= device
    count = 0
    for i in range(len(adapters)):
        if adapters[i] > output + 3:
            break
        count += combinations(device, adapters[i], adapters[i+1:], memory)
    memory[adapters] = count
    return count

def solve2(entries):
    # What is the total number of distinct ways you can arrange the adapters to
    # connect the charging outlet to your device?
    memory = {}
    return combinations(max(entries) + 3, 0, tuple(sorted(entries)), memory)

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """16
10
15
5
1
11
7
19
6
12
4
"""
    input2 = """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 7*5)
        self.assertEqual(solve1(parse_input(self.input2)), 22*10)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 8)
        self.assertEqual(solve2(parse_input(self.input2)), 19208)
if __name__ == "__main__":
    with open("input/day10.txt") as f:
        entries = parse_input(f.read())
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
