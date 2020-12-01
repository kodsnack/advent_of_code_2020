#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys

def part1(data):
    for element_a in data:
        for element_b in data:
            if element_a + element_b == 2020:
                return element_a * element_b
    return 0

def part2(data):
    return 0

## Unit tests ########################################################

class TestDay01(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([1721, 979, 366, 299, 675, 1456]), 514579)
 

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day X")
    print("Part1 result: {}".format(part1(list(getIntsFromFile(sys.argv[1])))))
    print("Part2 result: {}".format(part2(getIntsFromFile(sys.argv[1]))))
