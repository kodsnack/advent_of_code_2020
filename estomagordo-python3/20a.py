
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def solve(tiles):
    sides = {}

    for id, tile in tiles.items():
        up = {''.join((tile[0])), ''.join(reversed(tile[0]))}
        down = {''.join((tile[-1])), ''.join(reversed(tile[-1]))}
        left = {''.join([tile[row][0] for row in range(len(tile))]), ''.join(reversed([tile[row][0] for row in range(len(tile))]))}
        right = {''.join([tile[row][-1] for row in range(len(tile))]), ''.join(reversed([tile[row][-1] for row in range(len(tile))]))}

        sides[id] = (up, down, left, right)

    corners = []

    idlist = list(tiles.keys())

    for i in range(len(idlist)):
        a = sides[idlist[i]]
        matches = [False] * 4

        for j in range(len(idlist)):
            if i == j:
                continue

            b = sides[idlist[j]]

            for x in range(4):
                for y in range(4):
                    if a[x] & b[y]:
                        matches[x] = True

        if sum(matches) == 2:
            corners.append(idlist[i])

    return reduce(lambda a,b: a*b, corners)

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
