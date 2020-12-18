
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product, chain

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, n_neighs, manhattan, neighs, neighs_bounded

def solve(lines, repeats):
    active = {(x, y, 0, 0) for y, line in enumerate(lines) for x, c in enumerate(line) if c == '#'}

    for _ in range(repeats):
        counts = Counter(chain.from_iterable(n_neighs(a) for a in active))
        active = {k for k,v in counts.items() if v == 3 or (k in active and v == 2)}
    
    return len(active)

if __name__ == '__main__':
    lines = []

    with open('17.txt') as f:
        for line in f.readlines():
            lines.append(list(line.rstrip()))

    print(solve(lines, 6))
