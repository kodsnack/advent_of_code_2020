
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(lines):
    wayy = 1
    wayx = 10
    y = 0
    x = 0

    for instruction in lines:
        command = instruction[0]
        val = int(instruction[1:])

        if command == 'N':
            wayy += val
        if command == 'S':
            wayy -= val
        if command == 'E':
            wayx += val
        if command == 'W':
            wayx -= val
        if command == 'L':
            if val == 90:
                wayy, wayx = wayx, wayy * -1
            if val == 180:
                wayy, wayx = wayy * -1, wayx * -1
            if val == 270:
                wayy, wayx = wayx * -1, wayy
        if command == 'R':
            if val == 90:
                wayy, wayx = wayx * -1, wayy
            if val == 180:
                wayy, wayx = wayy * -1, wayx * -1
            if val == 270:
                wayy, wayx = wayx, wayy * -1
        if command == 'F':
            y += val * wayy
            x += val * wayx
    
    return manhattan((y, x), (0, 0))

if __name__ == '__main__':
    lines = []

    with open('12.txt') as f:
        for line in f.readlines():
            lines.append(line.strip())

    print(solve(lines))