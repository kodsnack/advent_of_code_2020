
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(nums):
    nums.append(0)
    nums.append(max(nums)+3)
    nums.sort()

    onediff = 0
    threediff = 0

    for x in range(1, len(nums)):
        if nums[x] - nums[x-1] == 1:
            onediff += 1
        if nums[x] - nums[x-1] == 3:
            threediff += 1

    return onediff * threediff

if __name__ == '__main__':
    nums = []

    with open('10.txt') as f:
        for line in f.readlines():
            nums.append(int(line))

    print(solve(nums))
