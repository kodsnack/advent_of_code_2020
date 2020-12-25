#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-25
# 06:41 p1 solved

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple


def parse_input(input):
    return [int(v) for v in input.strip().split()]


def transform(initial_value, subject_number, loop_size):
    value = initial_value
    for i in range(loop_size):
        value = (value * subject_number) % 20201227
    return value


# What encryption key is the handshake trying to establish?
def solve1(entries):
    print(entries)
    subject_number = 7
    loop_size = 1
    loop_sizes = {}
    public_key = 1
    while True:
        public_key = transform(public_key, subject_number, 1)
        if public_key in entries:
            print("Found public key", public_key, "loop_size", loop_size)
            loop_sizes[public_key] = loop_size
            if len(loop_sizes) == len(entries):
                break
        loop_size += 1
    return transform(1, entries[0], loop_sizes[entries[1]])


# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """5764801
17807724"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 14897079)


if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(solve1(entries))
