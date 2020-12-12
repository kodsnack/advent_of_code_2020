
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded

def rotate(facing, steps):
    if steps == 0:
        return facing

    if steps < 0:
        return rotate(facing, 4+steps)

    if facing == [0, 1]:
        return rotate([-1, 0], steps-1)
    if facing == [-1, 0]:
        return rotate([0, -1], steps-1)
    if facing == [0, -1]:
        return rotate([1, 0], steps-1)

    return rotate([0, 1], steps-1)

def solve(lines):
    facing = [0, 1]
    y = 0
    x = 0

    for instruction in lines:
        command = instruction[0]
        val = int(instruction[1:])

        if command == 'N':
            y += val
        if command == 'S':
            y -= val
        if command == 'E':
            x += val
        if command == 'W':
            x -= val
        if command == 'L':
            if val == 90:
                facing = rotate(facing, -1)
            if val == 180:
                facing = rotate(facing, -2)
            if val == 270:
                facing = rotate(facing, -3)
        if command == 'R':
            if val == 90:
                facing = rotate(facing, 1)
            if val == 180:
                facing = rotate(facing, 2)
            if val == 270:
                facing = rotate(facing, 3)
        if command == 'F':
            y += val * facing[0]
            x += val * facing[1]
    
    return manhattan((y, x), (0, 0))

if __name__ == '__main__':
    lines = []

    with open('12.txt') as f:
        for line in f.readlines():
            lines.append(line.strip())

    print(solve(lines))
