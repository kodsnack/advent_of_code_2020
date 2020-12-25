#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-12

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

Instr = namedtuple("Instr", ["op", "val"])

class Vec:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def mult(self, val):
        return Vec(self.x * val, self.y * val)

    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y)

    def __str__(self):
        return "({}, {})".format(self.x, self.y)

def parse_input(input):
    lines = input.strip().split("\n")
    entries = []
    for line in lines:
        m = re.match(r"([WNSELRF])(\d+)", line)
        entries.append(Instr(m.group(1), int(m.group(2))))
    return entries

def dir_vec(direction):
    directions = {'W':Vec(-1,0), 'N':Vec(0,1), 'E':Vec(1,0), 'S':Vec(0,-1)}
    return directions[direction]

def angle_from_dir(direction):
    directions = {'W':180, 'N':90, 'E':0, 'S':270}
    return directions[direction]

def dir_from_angle(angle):
    angles = {180:'W',90:'N',0:'E',270:'S'}
    return angles[angle % 360]

def turn(start, angle):
    return dir_from_angle(angle_from_dir(start) + angle)

def travel(pos, direction, e):
    if e.op in {'N', 'E', 'S', 'W'}:
        return (pos + dir_vec(e.op).mult(e.val), direction)
    elif e.op == 'F':
        return (pos + dir_vec(direction).mult(e.val), direction)
    elif e.op == 'L':
        return (pos, turn(direction, e.val))
    elif e.op == 'R':
        return (pos, turn(direction, -e.val))
    else:
        raise ValueError(f"Unsupported op: {e.op}")

def solve1(entries):
    pos = Vec(0, 0)
    direction = 'E'
    for e in entries:
        pos, direction = travel(pos, direction, e)
    return abs(pos.x) + abs(pos.y)

def sinus(angle):
    angles = {0:0, 90: 1, 180: 0, 270: -1}
    return angles[angle % 360]

def cosinus(angle):
    return sinus(angle + 90)

def rotate(v, angle):
    return Vec(v.x * cosinus(angle) - v.y * sinus(angle), v.x * sinus(angle) + v.y * cosinus(angle))

def travel2(ship, waypoint, e):
    if e.op in {'N', 'E', 'S', 'W'}:
        return (ship, waypoint + dir_vec(e.op).mult(e.val))
    elif e.op == 'F':
        return ((ship[0] + waypoint.mult(e.val), ship[1]), waypoint)
    elif e.op == 'L':
        return ((ship[0], ship[1]), rotate(waypoint, e.val))
    elif e.op == 'R':
        return ((ship[0], ship[1]), rotate(waypoint, -e.val))
    else:
        raise ValueError(f"Unsupported op: {e.op}")

def solve2(entries):
    ship = (Vec(0, 0), 'E')
    waypoint = Vec(10, 1)
    for e in entries:
        ship, waypoint = travel2(ship, waypoint, e)
    return abs(ship[0].x) + abs(ship[0].y)

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """F10
N3
F7
R90
F11
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 25)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 286)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
