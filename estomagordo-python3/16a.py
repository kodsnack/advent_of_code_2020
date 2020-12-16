
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(fields, other):
    tot = 0

    for ticket in other:
        for val in ticket:
            valid = False

            for field in fields:
                if (field[0] <= val <= abs(field[1]) or field[2] <= val <= -field[3]):
                    valid = True
                    break

            if not valid:
                tot += val

    return tot

if __name__ == '__main__':
    fields = []
    fieldsfound = False
    your = []
    other = []

    with open('16.txt') as f:
        for line in f.readlines():
            if not line.strip():
                fieldsfound = True
            if not fieldsfound:
                fields.append(ints(line))
            elif not your:
                your = ints(line)
            else:
                other.append(ints(line))

    print(solve(fields, other))