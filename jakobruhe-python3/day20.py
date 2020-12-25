#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-20
#
# The code has not been cleaned up.

import os
import re
import unittest
import math
import numpy as np
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    lines = input.strip().split("\n")
    tiles = {}
    i = 0
    num_tile_lines = 10
    while i < len(lines):
        m = re.match(r"Tile (\d+):", lines[i])
        tile_id = int(m.group(1))
        tile_lines = lines[i+1:i+num_tile_lines+1]
        tiles[tile_id] = tile_lines
        i += num_tile_lines + 2
    return tiles

def get_edge(tile, edge):
    if edge == 0:
        return tile[0]
    elif edge == 1:
        return "".join([line[0] for line in tile])
    elif edge == 2:
        return tile[-1]
    elif edge == 3:
        return "".join([line[-1] for line in tile])
    else:
        raise ValueError("edge", edge)

def solve1(tiles):
    edge_hashes = defaultdict(list)
    for i, tile in tiles.items():
        for e in range(4):
            edge = get_edge(tile, e)
            edge_hashes[hash(edge)].append(i)
            edge_hashes[hash(edge[::-1])].append(i)
    tile_unique_edge_count = defaultdict(int)
    for h,ids in edge_hashes.items():
        if len(ids) == 1:
            tile_unique_edge_count[ids[0]] += 1
    corner_tids = []
    for tid, count in tile_unique_edge_count.items():
        if count == 4:
            corner_tids.append(tid)
    res = 1
    for tid in corner_tids:
        res *= tid
    return res


def rotate_flip(tile, rotate, flip):
    rotate = rotate % 4
    res = tile
    for r in range(rotate):
        res = np.rot90(res)
    if flip:
        res = np.fliplr(res)
    return res


def np_get_edge(nptile, edge):
    if edge == 0:
        return nptile[0,:]
    elif edge == 1:
        return nptile[:,-1]
    elif edge == 2:
        return nptile[-1,:]
    elif edge == 3:
        return nptile[:,0]
    else:
        raise ValueError(edge)


def nptile_from_tile(tile):
    ar = []
    for line in tile:
        ar.append(list(map(lambda c: 1 if c == "#" else 0, line)))
    return np.array(ar)


def hash_edge(edge):
    return hash(tuple(edge))


def edge_as_str(edge):
    return "".join(["#" if c else "." for c in edge])


def solve2(tiles_):
    tiles = {}
    for i, t in tiles_.items():
        tiles[i] = nptile_from_tile(t)
    edge_hashes = defaultdict(list)
    for i, tile in tiles.items():
        for edge_id in range(4):
            edge = tuple(np_get_edge(tile, edge_id))
            edge_hashes[i].append(hash(edge))
            edge_hashes[i].append(hash(tuple(reversed(edge))))
    edge_hash_map = defaultdict(list)
    for tid, edges in edge_hashes.items():
        for i, h in enumerate(edges):
            edge_hash_map[h].append((tid, i))
    for h,ids in edge_hash_map.items():
        assert len(ids) <= 2
    tile_unique_edge_count = defaultdict(int)
    for h,ids in edge_hash_map.items():
        if len(ids) == 1:
            tile_unique_edge_count[ids[0][0]] += 1
    corner_tids = []
    for tid, count in tile_unique_edge_count.items():
        if count == 4:
            corner_tids.append(tid)

    # Start with a corner tile.
    # Rotate/flip until its unique edges are top and left.
    # Attach tiles on the right until end of line.
    # Tiles may have to be rotated and/or flipped h/v in order to attach correctly.
    num_tiles = len(tiles)
    size = int(math.sqrt(num_tiles))
    assert size*size == num_tiles
    tile_size = 10
    tiles_left = dict(tiles)

    # For the top left tile we want edge 0 and 3 to be unique.
    first_tile_id = corner_tids[0]
    first_tile_r = None
    print("first_tile", first_tile_id)
    for r in range(4):
        num = len(edge_hash_map[edge_hashes[first_tile_id][r * 2]])
        print("first_tile", first_tile_id, "r", r, "num", num)
        if num == 1 and first_tile_r is None:
            first_tile_r = r + 1
        elif first_tile_r is not None:
            assert num == 1
            break
    print("first_tile_r", first_tile_r)

    image_tiles = []
    used_tiles = set()

    for y in range(size):
        row = []
        for x in range(size):
            left = row[x-1] if x > 0 else None
            above = image_tiles[y-1][x] if y > 0 else None
            if left is None and above is None:
                row.append((first_tile_id, first_tile_r, 0))
                used_tiles.add(first_tile_id)
                continue
            left_edge = tuple(np_get_edge(rotate_flip(tiles[left[0]], left[1], left[2]), 1)) if left is not None else None
            left_hash = hash(left_edge) if left is not None else None
            above_edge = tuple(np_get_edge(rotate_flip(tiles[above[0]], above[1], above[2]), 2)) if above is not None else None
            above_hash = hash(above_edge) if above is not None else None
            attach_tile = None
            if left_hash is not None:
                for tile_edge in edge_hash_map[left_hash]:
                    if not tile_edge[0] in used_tiles:
                        attach_tile = tile_edge
                        break
                if attach_tile is None:
                    raise RuntimeError("No tile found to attach")
                if attach_tile is not None:
                    found = False
                    for i in range(8):
                        rotate = i // 2
                        flip = i % 2
                        edge = tuple(np_get_edge(rotate_flip(tiles[attach_tile[0]], rotate, flip), 3))
                        if edge == left_edge:
                            found = True
                            break
                    assert found
                    print("attaching tile", attach_tile[0], "rotate", rotate, "flip", flip)
                    row.append((attach_tile[0], rotate, flip))
                    used_tiles.add(attach_tile[0])
            elif above_hash is not None:
                for tile_edge in edge_hash_map[above_hash]:
                    if not tile_edge[0] in used_tiles:
                        attach_tile = tile_edge
                        break
                if attach_tile is None:
                    raise RuntimeError("No tile found to attach")
                if attach_tile is not None:
                    found = False
                    for i in range(8):
                        rotate = i // 2
                        flip = i % 2
                        edge = tuple(np_get_edge(rotate_flip(tiles[attach_tile[0]], rotate, flip), 0))
                        if edge == above_edge:
                            found = True
                            break
                    assert found
                    print("attaching tile", attach_tile[0], "rotate", rotate, "flip", flip)
                    row.append((attach_tile[0], rotate, flip))
                    used_tiles.add(attach_tile[0])
        image_tiles.append(row)

    print("Image with edges")
    ts = tile_size
    image_with_edges = np.zeros((size * ts, size * ts))
    for ty,row in enumerate(image_tiles):
        for tx,tile_rf in enumerate(row):
            tile = rotate_flip(tiles[tile_rf[0]], tile_rf[1], tile_rf[2])
            image_with_edges[ty*ts:ty*ts+ts,tx*ts:tx*ts+ts] = tile[:,:]
    for y in range(tile_size * size):
        print(edge_as_str(image_with_edges[y,:]))

    print("The image we have all been waiting for")
    ts = tile_size - 2
    image = np.zeros((size * ts, size * ts))
    for ty,row in enumerate(image_tiles):
        for tx,tile_rf in enumerate(row):
            tile = rotate_flip(tiles[tile_rf[0]], tile_rf[1], tile_rf[2])
            image[ty*ts:ty*ts+ts,tx*ts:tx*ts+ts] = tile[1:ts+1,1:ts+1]
    for y in range(ts * size):
        print(edge_as_str(image[y,:]))

    template_str = """
                  #.
#    ##    ##    ###
 #  #  #  #  #  #..."""
    template = nptile_from_tile(template_str.split("\n")[1:])

    num_monsters_max = None
    num_ones_max = None
    rf_max = None

    for rf in range(8):
        image_rf = rotate_flip(image, rf // 2, rf % 2)
        num_monsters = 0
        one_indices = np.where(template == 1)
        th, tw = template.shape
        for y in range(image_rf.shape[0] - th):
            for x in range(image_rf.shape[1] - tw):
                region = image_rf[y:y+th,x:x+tw]
                if np.all(region[one_indices]):
                    num_monsters += 1
        print("rf", rf, "num_monsters", num_monsters)
        num_ones = np.count_nonzero(image)
        if num_monsters > 0:
            break
    return num_ones - num_monsters * np.count_nonzero(template)



# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 20899048083289)
        pass
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 273)
        pass

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(solve1(entries))
    print(solve2(entries))
