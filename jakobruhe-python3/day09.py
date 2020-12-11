#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-09
# 06:18: p1 solved
# 06:31: p2 solved

import unittest

def parse_input(input):
    return list(map(int, input.strip().split("\n")))

def find_sum(numbers, value):
    for i in range(len(numbers)):
        for j in range(len(numbers)):
            if i != j and numbers[i] + numbers[j] == value:
                return True
    return False

def solve1(numbers, preamble_length=25):
    for i in range(preamble_length, len(numbers)):
        if not find_sum(numbers[i - preamble_length:i], numbers[i]):
            return numbers[i]
    return None

def solve2(numbers, invalid_number, preamble_length=25):
    for i in range(len(numbers)):
        current_sum = 0
        for j in range(i, len(numbers)):
            current_sum += numbers[j]
            if current_sum == invalid_number:
                return min(numbers[i:j+1]) + max(numbers[i:j+1])
            elif current_sum > invalid_number:
                break
    return None

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input), 5), 127)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input), 127, 5), 62)

if __name__ == "__main__":
    with open("input/day09.txt") as f:
        input = f.read()
    entries = parse_input(input)
    a1 = solve1(entries)
    print(f"{a1}")
    a2 = solve2(entries, a1)
    print(f"{a2}")
