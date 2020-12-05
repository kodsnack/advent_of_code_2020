
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, ints, manhattan, neighs, neighs_bounded


def solve(lines):
    best = 0

    for line in lines:
        minrow = 0
        maxrow = 127
        mincol = 0
        maxcol = 7

        for x in range(7):
            if line[x] == 'F':
                maxrow = (minrow + maxrow) // 2
            else:
                minrow = (minrow + maxrow) // 2 + 1

        for y in range(7, 10):
            if line[y] == 'L':
                maxcol = (mincol + maxcol) // 2
            else:
                mincol = (mincol + maxcol) // 2 + 1

        print(minrow == maxrow, mincol == maxcol)
        best = max(best, minrow * 8 + mincol)

    return best

if __name__ == '__main__':
    lines = []

    with open('5.txt') as f:
        for line in f.readlines():
            lines.append(line.strip())

    print(solve(lines))

# 897
