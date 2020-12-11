
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(nums, window_size=25):
    n = len(nums)

    for x in range(window_size, n):
        good = False

        for i in range(x-window_size, x-1):
            if good:
                break

            for j in range(i+1, x):
                if nums[i] + nums[j] == nums[x]:
                    good = True
                    break
                
        if not good:
            return nums[x]

if __name__ == '__main__':
    nums = []

    with open('9.txt') as f:
        for line in f.readlines():
            nums.append(int(line))

    print(solve(nums))
