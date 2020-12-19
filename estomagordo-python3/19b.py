
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
    endsforlen = [set() for _ in range(longest+1)]

    for m in messages:
        for x in range(1, len(m)):
            startsforlen[x].add(m[:x])
            endsforlen[x].add(m[-x:])

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

    # rules[11] = []

    # for s, iters in eight.items():
    #     for t in rules[31]:
    #         u = s + t * iters
    #         l = len(u)
    #         if l <= longest:
    #             rules[11].append(s + t * iters)
    
    # print(len(rules[11]))

    rules[0] = set()

    # for s, siters in eight.items():
    #     for t in rules[31]:
    #         for titers in range(1,40):
    #             u = s + t * titers
    #             l = len(u)
    #             if l <= longest:
    #                 rules[0].append(u)

    ses = set(rules[42])

    while True:
        newses = set()

        for s1 in ses:
            for s2 in rules[42]:
                u = s1+s2

                if u not in ses and len(u) <= longest and u in startsforlen[len(u)]:
                    newses.add(u)

        if not newses:
            break

        ses |= newses

    # print(len(ses))

    ts = set(rules[31])

    while True:
        newts = set()

        for s1 in ts:
            for s2 in rules[31]:
                u = s1+s2

                if u not in ts and len(u) <= longest and u in endsforlen[len(u)]:
                    newts.add(u)

        if not newts:
            break

        ts |= newts

    print(len(ts))
    print(endsforlen[10])
    print({m[-10:] for m in messages})
    print(endsforlen[10] - {m[-10:] for m in messages})
    rules[0] = [p[0] + p[1] for p in product(ses, ts)]

    # for p in product(ses, ts):
    #     a = 22


    # rules[0] = []

    # for s in ses:
    #     for t in ts:
    #         rules[0].append(s+t)

    # print(len(rules[0]))

    # print('ses')
    # for s in ses:
    #     print(s)

    # print('ts')
    # for t in ts:
    #     print(t)
    
    # for s in rules[42]:
    #     ses = set()

    # print('ends with 5')
    # for e in endsforlen[5]:
    #     print(e)

    # print('bbabbbbaabaabba' in rules[0])

    #     for t in rules[31]:

    # print(len(rules[0]), longest, len(rules[42]), len(rules[31]))
    # print(Counter(len(r) for r in rules[0]))
    # print(' '.join(str(len(m)) for m in messages))

    # print(42)
    # for r in rules[42]:
    #     print(r)

    # print(31)
    # for r in rules[31]:
    #     print(r)

    # for m in messages:
    #     print(m in rules[0])

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
    # for e in eight.keys():
    #     if e in 'baabbaaaabbaaaababbaababb':
    #         print('found', e, eight[e], 'baabbaaaabbaaaababbaababb'[len(e):], 'baabbaaaabbaaaababbaababb'[len(e):] in rules[31])
    
    # print('aaaab' in ses)
    # print('aaaabbaaaa' in ses)
    # print('aaaabbaaaabbaaa' in ses)
    # print('bbaaa' in ts)
    # print('baaaabbaaa' in ts)
    # print('aaaabbaaaabbaaa' in ts)

    # print('aaaab' in rules[42])
    # print('baaaa' in rules[42])
    # print('bbaaa' in rules[31])

    # print('weird aaaaa shit')
    # print('aaaaabbaabaaaaababaa' in ses)
    # print('aaaaabbaabaaaaa' in ses)
    # print('aaaaabbaab' in ses)
    # print('aaaaa' in ses)
    # print('aaaaabbaabaaaaababaa' in ts)
    # print('aaaaababaa' in ts)

    # for r in rules[31]:
    #     print(r)

    print('sesmatch')
    print('aaabbbbbbaaaabaababaabababbabaaabbababababaaa' in ses)
    print('aaabbbbbbaaaabaababaabababbabaaabbababab' in ses)
    print('aaabbbbbbaaaabaababaabababbabaaabba' in ses)
    print('aaabbbbbbaaaabaababaabababbaba' in ses)
    print('aaabbbbbbaaaabaababaababa' in ses)
    print('aaabbbbbbaaaabaababa' in ses)
    print('aaabbbbbbaaaaba' in ses)
    print('aaabbbbbba' in ses)
    print('aaabb' in ses)
    print('tsmatch')
    print('aaabbbbbbaaaabaababaabababbabaaabbababababaaa' in ts)
    print('bbbbaaaabaababaabababbabaaabbababababaaa' in ts)
    print('aaabaababaabababbabaaabbababababaaa' in ts)
    print('aaabaababaabababbabaaabbababababaaa' in ts)
    print('ababaabababbabaaabbababababaaa' in ts)
    print('abababbabaaabbababababaaa' in ts)
    print('bbabaaabbababababaaa' in ts)
    print('aabbababababaaa' in ts)
    print('babababaaa' in ts)
    print('abaaa' in ts)

    print('42 31 match check')
    print('bbaba' in rules[42])
    print('bbaba' in rules[31])
    print('aabba' in rules[42])
    print('aabba' in rules[31])
    print('babab' in rules[42])
    print('babab' in rules[31])
    print('abaaa' in rules[42])
    print('abaaa' in rules[31])

    print('matches')
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

# 360 too high