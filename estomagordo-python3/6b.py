
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(qs):
    count = 0

    for group in qs:
        s = set('abcdefghijklmnopqrstuvwxyz')

        for g in group:
            s &= set(g)

        count += len(s)

    return count

if __name__ == '__main__':
    qs = []
    group = []

    with open('6.txt') as f:
        for line in f.readlines():
            if not line.strip():
                qs.append(group)
                group = []
            else:
                group.append(line.rstrip())

    if group:
        qs.append(group)

    print(solve(qs))

# 1646