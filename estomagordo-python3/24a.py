
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded


def move(s, y, x):
    if s == 'e':
        return 0, 1
    if s == 'se':
        if y % 2:
            return -1, 0
        return -1, 1
    if s == 'sw':
        if y % 2:
            return -1, -1
        return -1, 0
    if s == 'w':
        return 0, -1
    if s == 'nw':
        if y % 2:
            return 1, -1
        return 1, 0
    elif y % 2:
        return 1, 0
    return 1, 1


def solve(lines):
    flipped = set()
    moves = { 'e', 'se', 'sw', 'w', 'nw', 'ne' }

    for instruction in lines:
        s = ''
        y = 0
        x = 0

        for c in instruction:
            s += c

            if s in moves:
                dy, dx = move(s, y, x)
                y += dy
                x += dx
                s = ''

        if (y, x) in flipped:
            flipped.remove((y, x))
        else:
            flipped.add((y, x))

    return len(flipped)

if __name__ == '__main__':
    lines = []

    with open('24.txt') as f:
        for line in f.readlines():
            lines.append(line.rstrip())

    print(solve(lines))