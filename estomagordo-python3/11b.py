
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(lines):
    height = len(lines)
    width = len(lines[0])

    while True:
        changed = False

        newlines = [list(line) for line in lines]

        for y in range(height):
            for x in range(width):
                c = lines[y][x]

                if c == '.':
                    continue

                occupied = 0

                dirs = eight_neighs(0, 0)

                for dy, dx in dirs:
                    diry = dy
                    dirx = dx

                    while 0 <= y+diry < height and 0 <= x+dirx < width:
                        if lines[y+diry][x+dirx] in 'L#':
                            if lines[y+diry][x+dirx] == '#':
                                occupied += 1
                            break

                        diry += dy
                        dirx += dx

                if c == 'L' and occupied == 0:
                    newlines[y][x] = '#'
                    changed = True
                if c == '#' and occupied >= 5:
                    newlines[y][x] = 'L'
                    changed = True

        if not changed:
            break
        
        lines = newlines

    return sum(sum(c == '#' for c in row) for row in lines)


if __name__ == '__main__':
    lines = []

    with open('11.txt') as f:
        for line in f.readlines():
            lines.append(line.rstrip())

    print(solve(lines))
