#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys

def getMissingFields(passport):
    requiredFields = ['ecl', 'pid', 'eyr', 'hcl', 'byr', 'iyr', 'cid', 'hgt']
    missingFields = {key for key in requiredFields if key not in passport}
    return missingFields

def isPassportValid(passport):
    missingFields = getMissingFields(passport)
    missingFields.discard('cid')
    return len(missingFields) == 0

@timing
def part1(data):
    return sum(1 for passport in data if isPassportValid(passport))

@timing
def part2(data):
    return 0

## Unit tests ########################################################

class TestDay04(unittest.TestCase):
    def testgetMissingFields1(self):
        self.assertEqual(getMissingFields({'ecl':'gry', 'pid':'860033327', 'eyr':'2020', 'hcl':'#fffffd', 'byr':1937,
                                           'iyr':2017, 'cid':147, 'hgt':'183cm'}), set())
    def testgetMissingFields2(self):
        self.assertEqual(getMissingFields({'iyr':2013, 'ecl':'amb', 'cid':350, 'eyr':2023, 'pid':'028048884',
                                           'hcl':'#cfa07d', 'byr':1929}), {'hgt'})
    def testgetMissingFields3(self):
        self.assertEqual(getMissingFields({'hcl':'#ae17e1', 'iyr':2013, 'eyr':2024, 'ecl':'brn', 'pid':760753108,
                                           'byr':1931, 'hgt':'179cm'}), {'cid'})
    def testgetMissingFields4(self):
        self.assertEqual(getMissingFields({'hcl':'#cfa07d', 'eyr':2025, 'pid':166559648, 'iyr':2011, 'ecl':'brn',
                                           'hgt':'59in'}), {'cid','byr'})
    def testIsPassPortValid1(self):
        self.assertEqual(isPassportValid({'ecl':'gry', 'pid':'860033327', 'eyr':'2020', 'hcl':'#fffffd', 'byr':1937,
                                              'iyr':2017, 'cid':147, 'hgt':'183cm'}), True)
    def testIsPassPortValid2(self):
        self.assertEqual(isPassportValid({'iyr':2013, 'ecl':'amb', 'cid':350, 'eyr':2023, 'pid':'028048884',
                                              'hcl':'#cfa07d', 'byr':1929}), False)
    def testIsPassPortValid3(self):
        self.assertEqual(isPassportValid({'hcl':'#ae17e1', 'iyr':2013, 'eyr':2024, 'ecl':'brn', 'pid':760753108,
                                              'byr':1931, 'hgt':'179cm'}), True)
    def testIsPassPortValid4(self):
        self.assertEqual(isPassportValid({'hcl':'#cfa07d', 'eyr':2025, 'pid':166559648, 'iyr':2011, 'ecl':'brn',
                                              'hgt':'59in'}), False)

    testData = [{'ecl':'gry', 'pid':860033327, 'eyr':2020, 'hcl':'#fffffd', 'byr':1937, 'iyr':2017, 'cid':147, 'hgt':'183cm'},
                {'iyr':2013, 'ecl':'amb', 'cid':350, 'eyr':2023, 'pid':'028048884', 'hcl':'#cfa07d', 'byr':1929},
                {'hcl':'#ae17e1', 'iyr':2013, 'eyr':2024, 'ecl':'brn', 'pid':760753108, 'byr':1931, 'hgt':'179cm'},
                {'hcl':'#cfa07d', 'eyr':2025, 'pid':166559648, 'iyr':2011, 'ecl':'brn', 'hgt':'59in'}]
    def test_part1(self):
        self.assertEqual(part1(self.testData), 2)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day X")
    print("Part1 result: {}".format(part1(getDictsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getDictsFromFile(sys.argv[1]))))