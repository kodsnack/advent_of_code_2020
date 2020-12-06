
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

        for line in passport:
            for pair in line:
                k, v = list(pair.split(':'))
                characteristics.add(k)

        if len(characteristics & required) == len(required):
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