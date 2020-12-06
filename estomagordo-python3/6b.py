
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(groups):
    return sum(
                len(
                    reduce(
                        lambda a,b: a&b,
                        map(set, group),
                        set (
                            [chr(x) for x in range(ord('a'), ord('z')+1)]
                        )
                    )
                )
                for group in groups
            )

if __name__ == '__main__':
    with open('6.txt') as f:
        groups = grouped_lines(f.readlines())
        print(solve(groups))