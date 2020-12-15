
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(nums, iters):
    spoken = defaultdict(list)
    prev = -1

    for x in range(iters):
        if x < len(nums):
            spoken[nums[x]].append(x)
            prev = nums[x]
            continue

        if len(spoken[prev]) == 1:
            spoken[0].append(x)
            prev = 0
        else:
            d = spoken[prev][-1] - spoken[prev][-2]
            spoken[d].append(x)
            prev = d

    return prev

if __name__ == '__main__':
    with open('15.txt') as f:
        for line in f.readlines():
            print(solve(ints(line), 2020))
