
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def solve(rulelist, messages):
    rules = {}

    for rule in rulelist:
        if '"' in rule:
            l = rule.split(':')
            n = int(l[0])
            let = l[1].split('"')[-2]
            rules[n] = [let]

    while len(rules) < len(rulelist):
        for rule in rulelist:
            l = rule.split(':')
            n = int(l[0])

            if n in rules:
                continue

            nums = ints(l[1])

            if any(i not in rules for i in nums):
                continue

            parts = l[1].split('|')
            variants = []

            for x, part in enumerate(parts):
                partnums = ints(part)

                ss = ['']

                for partnum in partnums:
                    newss = []
                    for p in product(ss, rules[partnum]):
                        newss.append(''.join(p))
                    ss = newss

                variants.extend(ss)

            rules[n] = variants

    return sum(m in rules[0] for m in messages)

if __name__ == '__main__':
    rules = []
    messages = []
    seenblank = False

    with open('19.txt') as f:
        for line in f.readlines():
            if not line.rstrip():
                seenblank = True
            else:
                if seenblank:
                    messages.append(line.rstrip())
                else:
                    rules.append(line.rstrip())

    print(solve(rules, messages))
