
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def solve(lines):
    t = 0

    for line in lines:
        stack = []
        nums = ''
        val = None
        add = True

        for c in line:
            if c.isdigit():
                nums += c
            elif nums:
                if val:
                    if add:
                        val += int(nums)
                    else:
                        val *= int(nums)
                else:
                    val = int(nums)
                nums = ''
            if c == '+':
                add = True
            elif c == '*':
                add = False
            if c == '(':
                stack.append((val, add))
                val = None
            if c == ')':
                prev = stack.pop()

                if prev[1]:
                    val += prev[0] if prev[0] else 0
                else:
                    val *= prev[0] if prev[0] else 1

        if nums:
            if add:
                val += int(nums)
            else:
                val *= int(nums)
        
        t += val

    return t

if __name__ == '__main__':
    lines = []

    with open('18.txt') as f:
        for line in f.readlines():
            lines.append(line.replace(' ', '').rstrip())

    print(solve(lines))