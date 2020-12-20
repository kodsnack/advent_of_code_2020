
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def opposite(side):
    if side == 0:
        return 1
    if side == 1:
        return 0
    if side == 2:
        return 3
    return 2


def flipx(image):
    return tuple([tuple(row[::-1]) for row in image])


def flipy(image):
    return tuple(tuple(row) for row in image[::-1])


def rot(image):
    # uglyfuckery

    im = []
    row = []

    for x in range(len(image[0])):
        for y in range(len(image)):
            row.append(image[y][x])
        im.append(row[::-1])
        row = []

    if row:
        im.append(row)

    return tuple(tuple(row) for row in im)


def flipturns(image):
    out = set()

    for turns in range(4):
        image = rot(image)
        out.add(image)
        out.add(flipx(image))
        out.add(flipy(image))

    return out


def edges(tile):
    return (''.join((tile[0])), ''.join((tile[-1])), ''.join([tile[row][0] for row in range(len(tile))]), ''.join([tile[row][-1] for row in range(len(tile))]))


def matches(a, b):
    return a == b or a == ''.join(reversed(b))


def solve(tiles):
    sides = {}

    for id, tile in tiles.items():
        up = {''.join((tile[0])), ''.join(reversed(tile[0]))}
        down = {''.join((tile[-1])), ''.join(reversed(tile[-1]))}
        left = {''.join([tile[row][0] for row in range(len(tile))]), ''.join(reversed([tile[row][0] for row in range(len(tile))]))}
        right = {''.join([tile[row][-1] for row in range(len(tile))]), ''.join(reversed([tile[row][-1] for row in range(len(tile))]))}

        sides[id] = (up, down, left, right)

    corners = []

    # for v in tiles.values():
    #     return len(flipturns(v))

    idlist = list(tiles.keys())

    graph = defaultdict(list)

    for i in range(len(idlist)):
        a = sides[idlist[i]]

        for j in range(len(idlist)):
            if i == j:
                continue

            b = sides[idlist[j]]

            for x in range(4):
                for y in range(4):
                    if a[x] & b[y]:
                        graph[idlist[i]].append(idlist[j])
                        # graph[idlist[i]].add((idlist[j], x))    

        if len(graph[idlist[i]]) == 2:
            corners.append(idlist[i])

    image = {corners[0] : (0, 0, edges(tiles[corners[0]]))}
    directions = [[1, 0], [-1, 0], [0, -1], [0, 1]]

    frontier = list(graph[corners[0]])

    for id in frontier:
        if id in image:
            continue

        # rotations = flipturns(tiles[id])

        up, down, left, right = edges(tiles[id])
        entry = None

        for tileid, tile in image.items():
            if tileid not in graph[id]:
                continue

            y, x, dedges = tile
            dup, ddown, dleft, dright = dedges

            # rot = -1

            if matches(up, dup):
                entry = (y-1, x, (down, up, right, left))
            if matches(up, ddown):
                entry = (y+1, x, (up, down, left, right))
            if matches(up, dleft):
                entry = (y, x-1, (left, right, down, up))
            if matches(up, dright):
                entry = (y, x+1, (right, left, up, down))

            if matches(down, dup):
                entry = (y-1, x, (up, down, left, right))
            if matches(down, ddown):
                entry = (y+1, x, (down, up, right, left))
            if matches(down, dleft):
                entry = (y, x-1, (right, left, up, down))
            if matches(down, dright):
                entry = (y, x+1, (left, right, down, up))

            if matches(left, dup):
                entry = (y-1, x, (right, left, up, down))
            if matches(left, ddown):
                entry = (y+1, x, (left, right, down, up))
            if matches(left, dleft):
                entry = (y, x-1, (down, up, right, left))
            if matches(left, dright):
                entry = (y, x+1, (up, down, left, right))

            if matches(right, dup):
                entry = (y-1, x, (left, right, down, up))
            if matches(right, ddown):
                entry = (y+1, x, (right, left, up, down))
            if matches(right, dleft):
                entry = (y, x-1, (up, down, left, right))
            if matches(right, dright):
                entry = (y, x+1, (down, up, right, left))

        image[id] = entry

        frontier += graph[id]

    ys = Counter(i[0] for i in image.values())
    xes = Counter(i[1] for i in image.values())

    print(ys)
    print(xes)

    # image = [['|' for _ in range(180)] for _ in range(180)]

    # image = {corners[0] : (0, 0)}
    # right, up = list(graph[corners[0]])

    # image[right[0]] = (0, 1)
    
    # neighs = graph[right[0]]
    # cornerdir = [rn[1] for rn in neighs if rn[0] in image][0]
    # rightdir = opposite(cornerdir)

    # x = 1

    # while rightdir in [neigh[1] for neigh in neighs]:
    #     cell = 



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
