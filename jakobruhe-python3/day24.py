#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-24

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple


DIR_XY = {"e":(2,0),"se":(1,-2),"sw":(-1,-2),"w":(-2,0),"nw":(-1,2),"ne":(1,2)}


def parse_input(input):
    lines = input.strip().split("\n")
    entries = []
    for line in lines:
        pos = 0
        entry = []
        while pos < len(line):
            identified = False
            for d in DIR_XY:
                if line.startswith(d, pos):
                    entry.append(d)
                    pos += len(d)
                    identified = True
                    break
            assert identified
        entries.append(tuple(entry))
    return tuple(entries)


def get_end_pos(instructions):
    pos = (0,0)
    for i in instructions:
        dir_xy = DIR_XY[i]
        pos = (pos[0] + dir_xy[0], pos[1] + dir_xy[1])
    return pos


def get_tiles(entries):
    tiles = {}
    for e in entries:
        pos = get_end_pos(e)
        if tiles.get(pos, "w") == "b":
            tiles[pos] = "w"
        else:
            tiles[pos] = "b"
    return tiles


def get_neighbors(pos):
    n = []
    for d in DIR_XY:
        dir_xy = DIR_XY[d]
        n.append((pos[0] + dir_xy[0], pos[1] + dir_xy[1]))
    return n


def get_num_black_neighbors(tiles, pos):
    num_black = 0
    for d in DIR_XY:
        dir_xy = DIR_XY[d]
        p = (pos[0] + dir_xy[0], pos[1] + dir_xy[1])
        if tiles.get(p, "w") == "b":
            num_black += 1
    return num_black


def get_num_black(tiles):
    return sum([t == "b" for t in tiles.values()])


def solve1(entries):
    return get_num_black(get_tiles(entries))


def solve2(entries):
    tiles = get_tiles(entries)
    for day in range(100):
        tiles_next = {}
        tiles_with_black_neighbors = defaultdict(int)
        for pos,c in tiles.items():
            num_black = get_num_black_neighbors(tiles, pos)
            # Any black tile with zero or more than 2 black tiles immediately
            # adjacent to it is flipped to white.
            if c == "b" and (num_black == 1 or num_black == 2):
                tiles_next[pos] = "b"
            if c == "b":
                neighbors = get_neighbors(pos)
                for n_pos in neighbors:
                    if tiles.get(n_pos, "w") == "w":
                        tiles_with_black_neighbors[n_pos] += 1
        # Any white tile with exactly 2 black tiles immediately adjacent to it
        # is flipped to black.
        for pos,count in tiles_with_black_neighbors.items():
            if count == 2:
                tiles_next[pos] = "b"
        tiles = tiles_next
        print("Day", day + 1, ":", get_num_black(tiles))
    return get_num_black(tiles)


# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
"""
    def test1(self):
        self.assertEqual(get_end_pos(parse_input("nwwswee")[0]), (0,0))
        self.assertEqual(get_end_pos(("w", "w")), (-4, 0))
        self.assertEqual(get_end_pos(("sw", "nw")), (-2, 0))
        self.assertEqual(get_end_pos(("e", "e", "sw", "w", "nw")), (0, 0))
        self.assertEqual(solve1(parse_input(self.input)), 10)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 2208)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(solve1(entries))
    print(solve2(entries))
