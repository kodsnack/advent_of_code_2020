
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def solve_straight(l):
    while '+' in l:
        pluspos = -1

        for i, c in enumerate(l):
            if c == '+':
                pluspos = i
                break

        l = l[:pluspos-1] + [l[pluspos-1] + l[pluspos+1]] + l[pluspos+2:]

    return reduce(lambda a,b: a*b, [v for v in l if isinstance(v, int)])


def calc_line(line):
        linel = list(line)

        for x in range(len(linel)):
            if linel[x].isdigit():
                linel[x] = int(linel[x])

        while True:
            parens = []
            stack = []

            for i, c in enumerate(linel):
                if c == '(':
                    stack.append(i)
                elif c == ')':
                    parens.append((stack.pop(), i))

            open = -1
            close = -1

            for o, c in parens:
                if not any(p[0] > o and p[1] < c for p in parens):
                    open = o
                    close = c
                    break

            if open == -1:
                break
            
            linel = linel[:open] + [solve_straight(linel[open+1:close])] + linel[close+1:]

        return solve_straight(linel)
    

def solve(lines):
    return sum(map(calc_line, lines))

if __name__ == '__main__':
    lines = []

    with open('18.txt') as f:
        for line in f.readlines():
            lines.append(line.replace(' ', '').rstrip())

    print(solve(lines))