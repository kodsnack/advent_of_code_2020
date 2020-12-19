
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def solve(rulelist, messages):
    for x, rule in enumerate(rulelist):
        if rule == '8: 42':
            rulelist[x] = '8: 42 | 42 8'
        elif rule == '11: 42 31':
            rulelist[x] = '11: 42 31 | 42 11 31'
    # for x, rule in enumerate(rulelist):
    #     if rule == '8: 42':
    #         rulelist[x] = '8: 42 | 42 42 | 42 42 42 | 42 42 42'
    #     elif rule == '11: 42 31':
    #         rulelist[x] = '11: 42 31 | 42 42 31 31'

    rules = {}

    for rule in rulelist:
        if '"' in rule:
            l = rule.split(':')
            n = int(l[0])
            let = l[1].split('"')[-2]
            rules[n] = [let]

    longest = max(len(m) for m in messages)

    while len(rules) < len(rulelist)-3:
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

    eight = {v: 1 for v in rules[42]}

    startsforlen = [set() for _ in range(longest+1)]

    for m in messages:
        for x in range(1, len(m)):
            startsforlen[x].add(m[:x])

    while True:
        neweight = {}
        done = True

        for s, i in eight.items():
            for t, j in eight.items():
                st = s + t
                l = len(st)
                if l <= longest and st in startsforlen[l]:
                    neweight[st] = i+j

        for s, i in neweight.items():
            if s not in eight:
                done = False
                eight[s] = i

        if done:
            break

    rules[8] = [e for e in eight.keys()]

    rules[11] = []

    for s, iters in eight.items():
        for t in rules[31]:
            u = s + t * iters
            l = len(u)
            if l <= longest:
                rules[11].append(s + t * iters)
    
    print(len(rules[11]))
    # for rule in rulelist:
    #     l = rule.split(':')
    #     n = int(l[0])

    #     if n in rules:
    #         continue

    #     nums = ints(l[1])

    #     if any(i not in rules for i in nums):
    #         continue

    #     parts = l[1].split('|')
    #     variants = []

    #     for x, part in enumerate(parts):
    #         partnums = ints(part)

    #         ss = ['']

    #         for partnum in partnums:
    #             newss = []
    #             for p in product(ss, rules[partnum]):
    #                 newss.append(''.join(p))
    #             ss = newss

    #         variants.extend(ss)

    #     rules[n] = variants
    # print(len(rules[31]))
    for e in eight.keys():
        if e in 'baabbaaaabbaaaababbaababb':
            print('found', e, eight[e], 'baabbaaaabbaaaababbaababb'[len(e):], 'baabbaaaabbaaaababbaababb'[len(e):] in rules[31])
    
    print('\n'.join(m for m in messages if m in rules[0]))    
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
