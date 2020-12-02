#!/usr/bin/env python3

from helpfunctions import *

import unittest, sys
import itertools, numpy

def find_parts(data, num_elements, expected_sum):
    for item in itertools.combinations(data, num_elements):
        if sum(item) == expected_sum:
            return item
    return ()

@timing
def part1(data):
    return numpy.prod(find_parts(data, 2, 2020))

@timing
def part2(data):
    return numpy.prod(find_parts(data, 3, 2020))

## Unit tests ########################################################

class TestDay01(unittest.TestCase):
    def test_find_parts_2_elements(self):
        self.assertEqual(find_parts([1721, 979, 366, 299, 675, 1456], 2, 2020), (1721, 299))

    def test_find_parts_3_elements(self):
        self.assertEqual(find_parts([1721, 979, 366, 299, 675, 1456], 3, 2020), (979, 366, 675))

    def test_part1(self):
        self.assertEqual(part1([1721, 979, 366, 299, 675, 1456]), 514579)

    def test_part2(self):
        self.assertEqual(part2([1721, 979, 366, 299, 675, 1456]), 241861950)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 1")
    print(f"Part 1 result: {part1(getIntsFromFile(sys.argv[1]))}")
    print(f"Part 2 result: {part2(getIntsFromFile(sys.argv[1]))}")
