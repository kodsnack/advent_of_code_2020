
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, ints, manhattan, neighs, neighs_bounded

def subs(lines, right, down):
    trees = 0
    y = 0
    x = 0

    while y < len(lines):
        if lines[y][x % len(lines[0])] == '#':
            trees += 1
        x += right
        y += down

    return trees


def solve(lines):
    nums = [subs(lines, 1, 1), subs(lines, 3, 1), subs(lines, 5, 1), subs(lines, 7, 1), subs(lines, 1, 2)]

    return reduce(lambda x,y: x*y, nums)

if __name__ == '__main__':
    lines = []

    with open('3.txt') as f:
        for line in f.readlines():
            lines.append(line.rstrip())

    print(solve(lines))
