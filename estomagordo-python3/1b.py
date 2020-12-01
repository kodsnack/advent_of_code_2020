
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, ints, manhattan, neighs, neighs_bounded


def solve(nums):
    n = len(nums)

    for i in range(n):
        a = nums[i]
        for j in range(i+1, n):
            b = nums[j]
            for k in range(j+1, n):
                c = nums[k]
                if a+b+c == 2020:
                    return(a*b*c)

if __name__ == '__main__':
    nums = []

    with open('1.txt') as f:
        for line in f.readlines():
            nums.append(int(line))

    print(solve(nums))
