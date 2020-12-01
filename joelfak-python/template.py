#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys

def test_calculate_something(data):
    return 2

def part1(data):
    return 0

def part2(data):
    return 0

## Unit tests ########################################################

class TestDayXX(unittest.TestCase):
    def test_calculate_something(self):
        self.assertEqual(test_calculate_something(1), 2)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day X")
    print("Part1 result: {}".format(part1(getIntsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getIntsFromFile(sys.argv[1]))))
