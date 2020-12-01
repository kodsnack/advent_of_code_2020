#!/usr/bin/env python3

from helpfunctions import *

import unittest, sys
from functools import wraps
from time import time

import itertools
import numpy

def timing(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print('Execution of:%r took: %2.4f sec' % \
          (f.__name__, te-ts))
        return result
    return wrap

@timing
def part1(data):
    for item in itertools.combinations(data, 2):
        if sum(item) == 2020:
            return numpy.prod(item)
    return 0

@timing
def part2(data):
    for item in itertools.combinations(data, 3):
        if sum(item) == 2020:
            return numpy.prod(item)
    return 0

## Unit tests ########################################################

class TestDay01(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([1721, 979, 366, 299, 675, 1456]), 514579)

    def test_part2(self):
        self.assertEqual(part2([1721, 979, 366, 299, 675, 1456]), 241861950)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 1")
    print(f"Part 1 result: {part1(getIntsFromFile(sys.argv[1]))}")
    print(f"Part 2 result: {part2(getIntsFromFile(sys.argv[1]))}")
