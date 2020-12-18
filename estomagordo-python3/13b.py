
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(rawbuses):
    buses = []

    for offset, val in enumerate(rawbuses):
        if val.isdigit():
            buses.append((offset, int(val)))

    start = 0
    step = buses[0][1]

    for offset, bus in buses[1:]:
        x = 0

        while (start + step * x + offset) % bus != 0:
            x += 1

        start += x * step
        step *= bus

    return start
            

if __name__ == '__main__':
    lines = open('13.txt').readlines()
    
    rawbuses = lines[1].split(',')

    print(solve(rawbuses))