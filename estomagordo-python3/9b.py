
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


def find_invalid(nums, window_size=25):
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


def solve(nums):
    target = find_invalid(nums)
    n = len(nums)

    for i in range(n):
        total = nums[i]

        for j in range(i+1, n):
            total += nums[j]
            
            if total == target:
                return min(nums[i:j+1]) + max(nums[i:j+1])
            if total > target:
                break

if __name__ == '__main__':
    nums = []

    with open('9.txt') as f:
        for line in f.readlines():
            nums.append(int(line))

    print(solve(nums))
