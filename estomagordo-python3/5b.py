
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, ints, manhattan, neighs, neighs_bounded


def binsearch(lo, hi, goleft, s):
    for c in s:
        midway = (lo + hi) // 2

        if c == goleft:
            hi = midway
        else:
            lo = midway+1

    return lo


def solve(lines):
    ids = set()

    for line in lines:
        row = binsearch(0, 127, 'F', line[:7])
        col = binsearch(0, 7, 'L', line[7:])
        id = row * 8 + col
        ids.add(id)

    return [x for x in range(min(ids), max(ids) + 1) if x not in ids][0]

if __name__ == '__main__':
    lines = []

    with open('5.txt') as f:
        for line in f.readlines():
            lines.append(line.strip())

    print(solve(lines))