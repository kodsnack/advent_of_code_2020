#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys

@timing
def part1(data):
    return 0

@timing
def part2(data):
    return 0

## Unit tests ########################################################

class TestDayXX(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1(1), 0)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day X")
    print("Part1 result: {}".format(part1(getIntsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getIntsFromFile(sys.argv[1]))))
