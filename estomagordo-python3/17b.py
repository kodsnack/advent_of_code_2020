
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded

def solve(lines, repeats):
    active = set()

    for y in range(len(lines)):
        for x in range(len(lines[0])):
            if lines[y][x] == '#':
                active.add((0, 0, y, x))

    for _ in range(repeats):
        minval = min(min(val for val in act) for act in active)
        maxval = max(max(val for val in act) for act in active)

        newact = set()
        
        for w in range(minval-1, maxval+2):
            for z in range(minval-1, maxval+2):
                for y in range(minval-1, maxval+2):
                    for x in range(minval-1, maxval+2):
                        actcount = 0

                        for wdiff in range(-1, 2):
                            for zdiff in range(-1, 2):
                                for ydiff in range(-1, 2):
                                    for xdiff in range(-1, 2):
                                        if wdiff == 0 and zdiff == 0 and ydiff == 0 and xdiff == 0:
                                            continue

                                        if (w+wdiff, z+zdiff, y+ydiff, x+xdiff) in active:
                                            actcount += 1

                        if actcount == 3 or (actcount == 2 and (w, z, y, x) in active):
                            newact.add((w, z, y, x))

        active = newact

    return len(active)

if __name__ == '__main__':
    lines = []

    with open('17.txt') as f:
        for line in f.readlines():
            lines.append(list(line.rstrip()))

    print(solve(lines, 6))
