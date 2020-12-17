
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded
from copy import deepcopy

def newlayer(height, width):
    return [['' for x in range(width)] for y in range(height)]



def solve(lines):
    # active = set()

    # for y in range(len(lines)):
    #     for x in range(len(lines[0])):
    #         if lines[y][x] == '#':
    #             active.add((0, y, x))

    cube = { 0: lines }

    for _ in range(6):
        minz = min(cube.keys())
        maxz = max(cube.keys())
        maxy = max(len(layer) for layer in cube.values())
        maxx = 0
        
        for layer in cube.values():
            for line in layer:
                maxx = max(maxx, len(line))

        for z in cube.keys():
            for y in range(len(cube[z])):
                cube[z][y] = ['.'] + cube[z][y] + ['.']

            cube[z] = [['.' for _ in range(maxx+2)]] + cube[z] + [['.' for _ in range(maxx+2)]]

        cube[minz-1] = newlayer(maxy+2, maxx+2)
        cube[maxz+1] = newlayer(maxy+2, maxx+2)

        newcube = deepcopy(cube)

        for z in cube.keys():
            layer = cube[z]
            for y in range(len(layer)):
                row = layer[y]
                for x in range(len(row)):
                    cell = row[x]
                    actneighs = 0

                    for dz in range(-1, 2):
                        for dy in range(-1, 2):
                            for dx in range(-1, 2):
                                if dz == 0 and dy == 0 and dx == 0:
                                    continue

                                if z+dz in cube and 0 <= y+dy < len(cube[z+dz]) and 0 <= x+dx < len(cube[z+dz][y+dy]) and cube[z+dz][y+dy][x+dx] == '#':
                                    actneighs += 1

                    if cell == '#':
                        if actneighs in (2, 3):
                            newcube[z][y][x] = '#'
                        else:
                            newcube[z][y][x] = '.'
                    elif actneighs == 3:
                        newcube[z][y][x] = '#'
                    else:
                        newcube[z][y][x] = '.'

        cube = newcube

    actcount = 0

    for layer in cube.values():
        for row in layer:
            for cell in row:
                if cell == '#':
                    actcount += 1

    return actcount

if __name__ == '__main__':
    lines = []

    with open('17.txt') as f:
        for line in f.readlines():
            lines.append(list(line.rstrip()))

    print(solve(lines))
