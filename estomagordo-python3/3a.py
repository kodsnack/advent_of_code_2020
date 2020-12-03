
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, ints, manhattan, neighs, neighs_bounded


def solve(lines):
    trees = 0
    y = 0
    x = 0

    while y < len(lines):
        if lines[y][x % len(lines[0])] == '#':
            trees += 1
        x += 3
        y += 1

    return trees

if __name__ == '__main__':
    lines = []

    with open('3.txt') as f:
        for line in f.readlines():
            lines.append(line.rstrip())

    print(solve(lines))
