
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(nums):
    nums.append(0)
    nums.append(max(nums)+3)
    nums.sort()
    n = len(nums)
    seen = {}

    def count(pos):
        if pos == n-1:
            return 1

        if pos in seen:
            return seen[pos]

        val = nums[pos]

        poss = count(pos+1)

        if pos < n-2 and nums[pos+2] <= nums[pos] + 3:
            poss += count(pos+2)
        if pos < n-3 and nums[pos+3] <= nums[pos] + 3:
            poss += count(pos+3)

        seen[pos] = poss

        return poss

    return count(0)

if __name__ == '__main__':
    nums = []

    with open('10.txt') as f:
        for line in f.readlines():
            nums.append(int(line))

    print(solve(nums))
