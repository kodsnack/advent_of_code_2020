
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(start, buses):
    best = [10**10, 10**10]

    for bus in buses:
        times = (start // bus) + 1

        wait = (times * bus) % start

        if wait < best[1]:
            best = [wait * bus, wait]

    return best[0]
            

if __name__ == '__main__':
    lines = open('13.txt').readlines()

    start = int(lines[0])
    buses = ints(lines[1])

    print(solve(start, buses))