
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded

def powmod(base, exp, m):
    result = 1

    while (exp > 0):
        if (exp & 1):
            result = (result * base) % m

        base = (base * base) % m
        exp >>= 1

    return result

def solve(keya, keyb):
    m = 20201227

    loopsa = 1
    subja = 7

    while True:
        vala = powmod(subja, loopsa, m)

        if vala == keya:
            break

        loopsa += 1

    valb = 1    

    for _ in range(loopsa):
        valb *= keyb
        valb %= m
            
    return valb

if __name__ == '__main__':
    keys = []

    with open('25.txt') as f:
        for line in f.readlines():
            keys.append(int(line))

    print(solve(keys[0], keys[1]))
