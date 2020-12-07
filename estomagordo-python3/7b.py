
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(d):
    seen = {}

    def count(colour):
        if colour in seen:
            return seen[colour]

        if colour not in d or not d[colour]:
            return 1

        amount = 0

        for pair in d[colour]:
            val = count(pair[1])
            if val > 1:
                val += 1
            amount += pair[0] * val

        seen[colour] = amount
        return amount

    return count('shiny gold')

if __name__ == '__main__':
    d = {}

    with open('7.txt') as f:
        for line in f.readlines():
            items = line.split()
            name = f'{items[0]} {items[1]}'
            contains = set()
            
            for i in range(4, len(items), 4):
                if items[i].isdigit():
                    contains.add((int(items[i]), f'{items[i+1]} {items[i+2]}'))
            
            d[name] = contains

    print(solve(d))