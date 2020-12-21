
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded


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

    for _ in range(4):
        image = rot(image)
        out.add(image)
        out.add(flipy(image))

    return out


def edges(tile):
    return (
            ''.join((tile[0])),
            ''.join((tile[-1])),
            ''.join([tile[row][0] for row in range(len(tile))]),
            ''.join([tile[row][-1] for row in range(len(tile))])
            )


def matches(a, b):
    return a == b or a == ''.join(reversed(b))


def count_monsters(image):
    monster = [
        '                  # ',
        '#    ##    ##    ###',
        ' #  #  #  #  #  #   '
    ]

    monsterflips = flipturns(monster)

    squares = {(y, x) for y in range(len(image)) for x in range(len(image[0])) if image[y][x] == '#'}
    monstered = set()

    height = len(image)
    width = len(image[0])

    for m in monsterflips:
        mheight = len(m)
        mwidth = len(m[0])

        for y in range(height-mheight+1):
            for x in range(width-mwidth+1):
                match = True
                hit = set()

                for dy in range(mheight):
                    for dx in range(mwidth):
                        if m[dy][dx] != '#':
                            continue
                        if image[y+dy][x+dx] != '#':
                            match = False
                        else:
                            hit.add((y+dy, x+dx))

                if match:
                    monstered |= hit
    
    return len(squares - monstered)


def solve(tiles):
    sides = {}

    for id, tile in tiles.items():
        sides[id] = edges(tile)

    graph = defaultdict(list)

    for a, b in product(sides.keys(), repeat=2):
        if a == b:
            continue
        
        for aside, bside in product(sides[a], sides[b]):
            if matches(aside, bside):
                graph[a].append(b)

    corners = [k for k,v in graph.items() if len(v) == 2]
    
    image = {corners[0] : (0, 0)}
    node = corners[0]
    y = 0
    x = 1
    cornercount = 1

    while any(n not in image and len(graph[n]) < 4 for n in graph[node]):
        for neighbour in graph[node]:
            if neighbour not in image and len(graph[neighbour]) < 4:
                image[neighbour] = (y, x)
                node = neighbour

                if len(graph[node]) == 2:
                    cornercount += 1

                if cornercount == 1:
                    x += 1
                elif cornercount == 2:
                    y += 1
                elif cornercount == 3:
                    x -= 1
                elif cornercount == 4:
                    y -= 1

                break    

    height = max(val[0] for val in image.values()) + 1
    width = max(val[1] for val in image.values()) + 1

    for x in range(1, width-1):
        for y in range(1, height-1):
            left = [k for k,v in image.items() if v == (y, x-1)][0]
            up = [k for k,v in image.items() if v == (y-1, x)][0]
            
            intersection = [id for id in graph.keys() if id in graph[left] and id in graph[up] and id not in image][0]

            image[intersection] = (y, x)
            
    coord_to_id = { v: k for k, v in image.items() }
    
    tileheight = len(tiles[corners[0]])
    tilewidth = len(tiles[corners[0]][0])

    canvas = [[' ' for _ in range((tilewidth-2) * width)] for _ in range((tileheight-2) * height)]
    
    for y in range(height):
        for x in range(width):
            id = coord_to_id[(y, x)]
            tile = tiles[id]

            up, down, left, right = edges(tile)

            if x < width-1:
                rid = coord_to_id[(y, x+1)]
                rtile = tiles[rid]
                redges = edges(rtile)

                while not any(matches(right, edge) for edge in redges):
                    tile = rot(tile)
                    up, down, left, right = edges(tile)
            else:
                lid = coord_to_id[(y, x-1)]
                ltile = tiles[lid]
                ledges = edges(ltile)

                while not any(matches(left, edge) for edge in ledges):
                    tile = rot(tile)
                    up, down, left, right = edges(tile)

            if y < height-1:
                did = coord_to_id[(y+1, x)]
                dtile = tiles[did]
                dedges = edges(dtile)

                if not any(matches(down, edge) for edge in dedges):
                    tile = flipy(tile)
            else:
                uid = coord_to_id[(y-1, x)]
                utile = tiles[uid]
                uedges = edges(utile)

                if not any(matches(up, edge) for edge in uedges):
                    tile = flipy(tile)
            
            starty = y * (tileheight - 2)
            startx = x * (tilewidth - 2)

            for dy in range(tileheight-2):
                for dx in range(tilewidth-2):
                    canvas[starty+dy][startx+dx] = tile[dy+1][dx+1]

    return count_monsters(canvas)


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
