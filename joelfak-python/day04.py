#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys

def getMissingFields(passport):
    requiredFields = ['ecl', 'pid', 'eyr', 'hcl', 'byr', 'iyr', 'cid', 'hgt']
    missingFields = {key for key in requiredFields if key not in passport}
    return missingFields

def isPassportComplete(passport):
    missingFields = getMissingFields(passport)
    missingFields.discard('cid')
    return len(missingFields) == 0

def isPassportFieldValid(key, value):
    if key == 'byr':
        return isinstance(value, int) and value >= 1920 and value <= 2002
    if key == 'iyr':
        return isinstance(value, int) and value >= 2010 and value <= 2020
    if key == 'eyr':
        return isinstance(value, int) and value >= 2020 and value <= 2030
    if key == 'hgt':
        try:
            if value[-2:] == 'cm':
                return int(value[:-2]) >= 150 and int(value[:-2]) <= 193
            if value[-2:] == 'in':
                return int(value[:-2]) >= 59 and int(value[:-2]) <= 76
        except (ValueError, TypeError):
            return False
    if key == 'hcl':
        valid_chars = '0123456789abcdef'
        return isinstance(value, str) and \
               len(value) == 7 and \
               value[0] == '#' and \
               all(c in valid_chars for c in value[1:])
    if key == 'ecl':
        valid_colors = ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']
        return isinstance(value, str) and value in valid_colors
    if key == 'pid':
        value = str(value)
        try:
            return len(value) == 9 and all(isinstance(int(x), int) for x in value)
        except ValueError:
            return False
    if key == 'cid':
        return True

    return False

def getInvalidFields(passport):
    return dict(filter(lambda f: not isPassportFieldValid(f[0], f[1]), passport.items()))

def isPassportValid(passport):
    passport_is_valid = all(isPassportFieldValid(f[0], f[1]) for f in passport.items())
    return passport_is_valid

@timing
def part1(data):
    return sum(1 for passport in data if isPassportComplete(passport))

@timing
def part2(data):
    return sum(1 for passport in data if isPassportComplete(passport) and isPassportValid(passport))

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
    def testIsPassportComplete1(self):
        self.assertEqual(isPassportComplete({'ecl':'gry', 'pid':'860033327', 'eyr':'2020', 'hcl':'#fffffd', 'byr':1937,
                                              'iyr':2017, 'cid':147, 'hgt':'183cm'}), True)
    def testIsPassportComplete2(self):
        self.assertEqual(isPassportComplete({'iyr':2013, 'ecl':'amb', 'cid':350, 'eyr':2023, 'pid':'028048884',
                                              'hcl':'#cfa07d', 'byr':1929}), False)
    def testIsPassportComplete3(self):
        self.assertEqual(isPassportComplete({'hcl':'#ae17e1', 'iyr':2013, 'eyr':2024, 'ecl':'brn', 'pid':760753108,
                                              'byr':1931, 'hgt':'179cm'}), True)
    def testIsPassportComplete4(self):
        self.assertEqual(isPassportComplete({'hcl':'#cfa07d', 'eyr':2025, 'pid':166559648, 'iyr':2011, 'ecl':'brn',
                                              'hgt':'59in'}), False)

    testData = [{'ecl':'gry', 'pid':860033327, 'eyr':2020, 'hcl':'#fffffd', 'byr':1937, 'iyr':2017, 'cid':147, 'hgt':'183cm'},
                {'iyr':2013, 'ecl':'amb', 'cid':350, 'eyr':2023, 'pid':'028048884', 'hcl':'#cfa07d', 'byr':1929},
                {'hcl':'#ae17e1', 'iyr':2013, 'eyr':2024, 'ecl':'brn', 'pid':760753108, 'byr':1931, 'hgt':'179cm'},
                {'hcl':'#cfa07d', 'eyr':2025, 'pid':166559648, 'iyr':2011, 'ecl':'brn', 'hgt':'59in'}]
    def test_part1(self):
        self.assertEqual(part1(self.testData), 2)


    def testIsPassportFieldValid_byrInvalidValues(self):
        self.assertFalse(isPassportFieldValid('byr', 173))
        self.assertFalse(isPassportFieldValid('byr', 1919))
        self.assertFalse(isPassportFieldValid('byr', 2003))
        self.assertFalse(isPassportFieldValid('byr', 23789))
        self.assertFalse(isPassportFieldValid('byr', 'asda'))

    def testIsPassportFieldValid_byrValidValues(self):
        self.assertTrue(isPassportFieldValid('byr', 1920))
        self.assertTrue(isPassportFieldValid('byr', 1978))
        self.assertTrue(isPassportFieldValid('byr', 2002))

    def testIsPassportFieldValid_iyrInvalidValues(self):
        self.assertFalse(isPassportFieldValid('iyr', 173))
        self.assertFalse(isPassportFieldValid('iyr', 2009))
        self.assertFalse(isPassportFieldValid('iyr', 2021))
        self.assertFalse(isPassportFieldValid('iyr', 32435))
        self.assertFalse(isPassportFieldValid('iyr', 'asda'))

    def testIsPassportFieldValid_iyrValidValues(self):
        self.assertTrue(isPassportFieldValid('iyr', 2010))
        self.assertTrue(isPassportFieldValid('iyr', 2015))
        self.assertTrue(isPassportFieldValid('iyr', 2020))

    def testIsPassportFieldValid_eyrInvalidValues(self):
        self.assertFalse(isPassportFieldValid('eyr', 173))
        self.assertFalse(isPassportFieldValid('eyr', 2019))
        self.assertFalse(isPassportFieldValid('eyr', 2031))
        self.assertFalse(isPassportFieldValid('eyr', 32435))
        self.assertFalse(isPassportFieldValid('eyr', 'asda'))

    def testIsPassportFieldValid_eyrValidValues(self):
        self.assertTrue(isPassportFieldValid('eyr', 2020))
        self.assertTrue(isPassportFieldValid('eyr', 2025))
        self.assertTrue(isPassportFieldValid('eyr', 2030))

    def testIsPassportFieldValid_hgtCmInvalidValues(self):
        self.assertFalse(isPassportFieldValid('hgt', '34cm'))
        self.assertFalse(isPassportFieldValid('hgt', '149cm'))
        self.assertFalse(isPassportFieldValid('hgt', '194cm'))
        self.assertFalse(isPassportFieldValid('hgt', '3252cm'))

    def testIsPassportFieldValid_hgtCmValidValues(self):
        self.assertTrue(isPassportFieldValid('hgt', '150cm'))
        self.assertTrue(isPassportFieldValid('hgt', '177cm'))
        self.assertTrue(isPassportFieldValid('hgt', '193cm'))

    def testIsPassportFieldValid_hgtInInvalidValues(self):
        self.assertFalse(isPassportFieldValid('hgt', '8in'))
        self.assertFalse(isPassportFieldValid('hgt', '58in'))
        self.assertFalse(isPassportFieldValid('hgt', '77in'))
        self.assertFalse(isPassportFieldValid('hgt', '325in'))

    def testIsPassportFieldValid_hgtInValidValues(self):
        self.assertTrue(isPassportFieldValid('hgt', '59in'))
        self.assertTrue(isPassportFieldValid('hgt', '65in'))
        self.assertTrue(isPassportFieldValid('hgt', '76in'))

    def testIsPassportFieldValid_hgtInvalidValues(self):
        self.assertFalse(isPassportFieldValid('hgt', '150gf'))
        self.assertFalse(isPassportFieldValid('hgt', '150'))
        self.assertFalse(isPassportFieldValid('hgt', 'in'))
        self.assertFalse(isPassportFieldValid('hgt', 150))

    def testIsPassportFieldValid_hclInvalidValues(self):
        self.assertFalse(isPassportFieldValid('hcl', '123456'))
        self.assertFalse(isPassportFieldValid('hcl', 123456))
        self.assertFalse(isPassportFieldValid('hcl', '#678abg'))

    def testIsPassportFieldValid_hclValidValues(self):
        self.assertTrue(isPassportFieldValid('hcl', '#012345'))
        self.assertTrue(isPassportFieldValid('hcl', '#456789'))
        self.assertTrue(isPassportFieldValid('hcl', '#abcdef'))
        self.assertTrue(isPassportFieldValid('hcl', '#1a0b3c'))

    def testIsPassportFieldValid_eclInvalidValues(self):
        self.assertFalse(isPassportFieldValid('ecl', 'hjk'))
        self.assertFalse(isPassportFieldValid('ecl', 829))
        self.assertFalse(isPassportFieldValid('ecl', 'ambr'))

    def testIsPassportFieldValid_eclValidValues(self):
        self.assertTrue(isPassportFieldValid('ecl', 'amb'))
        self.assertTrue(isPassportFieldValid('ecl', 'blu'))
        self.assertTrue(isPassportFieldValid('ecl', 'brn'))
        self.assertTrue(isPassportFieldValid('ecl', 'gry'))
        self.assertTrue(isPassportFieldValid('ecl', 'grn'))
        self.assertTrue(isPassportFieldValid('ecl', 'hzl'))
        self.assertTrue(isPassportFieldValid('ecl', 'oth'))

    def testIsPassportFieldValid_pidInvalidValues(self):
        self.assertFalse(isPassportFieldValid('pid', '12345678'))
        self.assertFalse(isPassportFieldValid('pid', '1234567867'))
        self.assertFalse(isPassportFieldValid('pid', '12a389045'))

    def testIsPassportFieldValid_pidValidValues(self):
        self.assertTrue(isPassportFieldValid('pid', '890678456'))
        self.assertTrue(isPassportFieldValid('pid', '767847679'))
        self.assertTrue(isPassportFieldValid('pid', '008657654'))
        self.assertTrue(isPassportFieldValid('pid', 123456789))

    def testIsPassportFieldValid_cidValidValues(self):
        self.assertTrue(isPassportFieldValid('cid', 'asdas'))
        self.assertTrue(isPassportFieldValid('cid', 'jsadkl7894asf'))
        self.assertTrue(isPassportFieldValid('cid', 8657654))
        self.assertTrue(isPassportFieldValid('cid', '00123456789'))

    def testIsPassportValid_invalidPassports(self):
        self.assertFalse(isPassportValid({'eyr':'1972', 'cid':100, 'hcl':'#18171d', 'ecl':'amb', 'hgt':170, 'pid':'186cm', 'iyr':2018, 'byr':1926}))
        self.assertFalse(isPassportValid({'iyr':2019, 'hcl':'#602927', 'eyr':1967, 'hgt':'170cm', 'ecl':'grn', 'pid':'012533040', 'byr':1946}))
        self.assertFalse(isPassportValid({'hcl':'dab227', 'iyr':2012, 'ecl':'brn', 'hgt':'182cm', 'pid':'021572410', 'eyr':2020, 'byr':1992, 'cid':277}))
        self.assertFalse(isPassportValid({'hgt':'59cm', 'ecl':'zzz', 'eyr':2038, 'hcl':'74454a', 'iyr':2023, 'pid':'3556412378', 'byr':2007}))

    def testIsPassportValid_validPassports(self):
        self.assertTrue(isPassportValid({'pid':'087499704', 'hgt':'74in', 'ecl':'grn', 'iyr':2012, 'eyr':2030, 'byr':1980, 'hcl':'#623a2f'}))
        self.assertTrue(isPassportValid({'eyr':2029, 'ecl':'blu', 'cid':129, 'byr':1989, 'iyr':2014, 'pid':'896056539', 'hcl':'#a97842', 'hgt':'165cm'}))
        self.assertTrue(isPassportValid({'hcl':'#888785', 'hgt':'164cm', 'byr':2001, 'iyr':2015, 'cid':88, 'pid':'545766238', 'ecl':'hzl', 'eyr':2022}))
        self.assertTrue(isPassportValid({'iyr':2010, 'hgt':'158cm', 'hcl':'#b6652a', 'ecl':'blu', 'byr':1944, 'eyr':2021, 'pid':'093154719'}))

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day X")
    print("Part1 result: {}".format(part1(getDictsFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getDictsFromFile(sys.argv[1]))))