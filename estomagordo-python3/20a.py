
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded


def edges(tile):
    return (''.join((tile[0])), ''.join((tile[-1])), ''.join([tile[row][0] for row in range(len(tile))]), ''.join([tile[row][-1] for row in range(len(tile))]))


def matches(a, b):
    return a == b or a == ''.join(reversed(b))


def solve(tiles):
    sides = {}

    for id, tile in tiles.items():
        sides[id] = edges(tile)

    matchcount = defaultdict(int)

    for a, b in product(sides.keys(), repeat=2):
        if a == b:
            continue
        
        for aside, bside in product(sides[a], sides[b]):
            if matches(aside, bside):
                matchcount[a] += 1
                
    return multall(k for k,v in matchcount.items() if v == 2)
   

if __name__ == '__main__':
    tiles = {}
    tile = []
    id = -1

    with open('20.txt') as f:
        for line in f.readlines():
            if line.strip():
                if ints(line):
                    id = ints(line)[0]
                else:
                    tile.append(line.strip())
            else:
                tiles[id] = tile
                id = -1
                tile = []

    tiles[id] = tile

    print(solve(tiles))
