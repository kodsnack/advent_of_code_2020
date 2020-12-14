
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def get_addresses(addr, mask):
    binary = bin(addr)[2:]
    binary = list('0' * (len(mask) - len(binary)) + binary)

    addresses = []
    wildcards = {}

    for pos, bit in enumerate(mask):
        if bit == '1':
            binary[pos] = '1'
        elif bit == 'X':
            wildcards[pos] = len(wildcards)

    for x in range(2**(len(wildcards))):
        posbin = bin(x)[2:]
        posbin = '0' * (len(wildcards) - len(posbin)) + posbin
        variant = []

        for pos, bit in enumerate(binary):
            if pos in wildcards:
                variant.append(posbin[wildcards[pos]])
            else:
                variant.append(binary[pos])

        addresses.append(int(''.join(variant), 2))

    return addresses


def solve(instructions):
    memory = defaultdict(int)
    mask = ''

    for ins in instructions:
        if 'mask' in ins:
            mask = ins.split()[-1]
            continue

        addr, val = ins
        binary = bin(val)[2:]
        binary = '0' * (len(mask) - len(binary)) + binary
        binlen = len(binary)
        final = []

        for binpos in range(-1, -binlen-1, -1):
            if mask[binpos] == 'X':
                final.append(binary[binpos])
            else:
                final.append(mask[binpos])

        rev = ''.join(list(final)[::-1])

        for address in get_addresses(addr, mask):
            memory[address] = val

    return sum(memory.values())

if __name__ == '__main__':
    instructions = []

    with open('14.txt') as f:
        for line in f.readlines():
            if 'mask' in line:
                instructions.append(line.rstrip())
            else:
                instructions.append(ints(line))

    print(solve(instructions))