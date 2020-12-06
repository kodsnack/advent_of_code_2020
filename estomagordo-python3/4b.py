
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, ints, manhattan, neighs, neighs_bounded


def solve(passports):
    required = { 'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid' }

    valid = 0

    for passport in passports:
        characteristics = set()
        good = True

        for line in passport:
            for pair in line:
                k, v = list(pair.split(':'))
                characteristics.add(k)
                if k == 'byr':
                    if len(v) != 4 or not 1920 <= int(v) <= 2002:
                        good = False
                if k == 'iyr':
                    if len(v) != 4 or not 2010 <= int(v) <= 2020:
                        good = False
                if k == 'eyr':
                    if len(v) != 4 or not 2020 <= int(v) <= 2030:
                        good = False
                if k == 'hgt':
                    nums = ints(v)
                    cm = len(v) > 1 and v[-2:] == 'cm'
                    inch = len(v) > 1 and v[-2:] == 'in'

                    if not (cm or inch):
                        good = False

                    if cm and not 150 <= nums[0] <= 193:
                        good = False
                    if inch and not 59 <= nums[0] <= 76:
                        good = False
                if k == 'hcl':
                    if len(v) != 7 or v[0] != '#' or any(c not in '0123456789abcdef' for c in v[1:]):
                        good = False
                if k == 'pid':
                    if len(v) != 9 or any(c not in '0123456789' for c in v):
                        good = False
                if k == 'ecl':
                    if v not in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']:
                        good = False

        if good and len(characteristics & required) == len(required):
            valid += 1

    return valid

if __name__ == '__main__':
    passports = []
    passport = []

    with open('4.txt') as f:
        for line in f.readlines():
            if not line.strip():
                passports.append(passport)
                passport = []
            else:
                passport.append(line.split())

    if passport:
        passports.append(passport)

    print(solve(passports))