
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import allints, distance, distance_sq, ints, manhattan, neighs, neighs_bounded


def solve(lines):
    count = 0

    for minmax, ccol, s in lines:
        mini, maxi = list(map(int, minmax.split('-')))
        c = ccol[:-1]

        if (s[mini-1] == c) ^ (s[maxi-1] == c):
            count += 1

    return count

if __name__ == '__main__':
    lines = []

    with open('2.txt') as f:
        for line in f.readlines():
            lines.append(line.split())

    print(solve(lines))
