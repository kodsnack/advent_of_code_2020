
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(d):
    targets = {'shiny gold'}
    good = set()

    while True:
        newtargets = set()

        for k, v in d.items():
            if (v & targets) and (k not in targets):
                newtargets.add(k)
        
        if not newtargets:
            break

        targets |= newtargets

    return len(targets)-1

if __name__ == '__main__':
    d = {}

    with open('7.txt') as f:
        for line in f.readlines():
            items = line.split()
            name = f'{items[0]} {items[1]}'
            contains = set()
            
            for i in range(5, len(items), 4):
                contains.add(f'{items[i]} {items[i+1]}')

            if ('other bags.') in contains:
                contains.remove('other bags.')
            
            d[name] = contains

    print(solve(d))