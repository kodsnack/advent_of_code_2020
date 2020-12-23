
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded


def solve(labeling, moves):
    cups = list(map(int, list(labeling)))
    highest = max(cups)
    currindex = 0
    n = len(cups)

    for _ in range(moves):
        currval = cups[currindex]
        picked = []

        for x in range(currindex+1, currindex+4):
            picked.append(cups[x%n])

        destlabel = cups[currindex]-1

        start = 0 if currindex < n-3 else currindex-n+4
        cups = cups[start:currindex+1] + cups[currindex+4:]

        while destlabel not in cups and destlabel > 1:
            destlabel -= 1

        if destlabel not in cups:
            destlabel = highest
            while destlabel not in cups:
                destlabel -= 1

        for x in range(n-3):
            if cups[x] == destlabel:
                cups = cups[:x+1] + picked + cups[x+1:]
                break

        if cups[-1] == currval:
            currindex = 0
        else:
            for x, cup in enumerate(cups):
                if cup == currval:
                    currindex = x+1
                    break

    out = []
    onefound = False

    for cup in cups:
        if cup == 1:
            onefound = True
        elif onefound:
            out.append(str(cup))

    for cup in cups:
        if cup == 1:
            break
        out.append(str(cup))
    
    return ''.join(out)

if __name__ == '__main__':
    print(solve('389547612', 100))