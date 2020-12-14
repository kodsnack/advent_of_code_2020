
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(instructions):
    memory = defaultdict(int)
    mask = ''

    for ins in instructions:
        if len(ins) == 1:
            mask = ins[0]
            continue

        addr, val = ins
        valbin = bin(val)[2:]
        valbin = '0' * (len(mask) - len(valbin)) + valbin
        binlen = len(valbin)
        final = []

        for binpos in range(-1, -binlen-1, -1):
            if mask[binpos] == 'X':
                final.append(valbin[binpos])
            else:
                final.append(mask[binpos])

        rev = ''.join(list(final)[::-1])

        memory[addr] = int(rev, 2)

    return sum(memory.values())

if __name__ == '__main__':
    instructions = []

    with open('14.txt') as f:
        for line in f.readlines():
            if 'mask' in line:
                instructions.append([line.split()[-1]])
            else:
                instructions.append(ints(line))

    print(solve(instructions))