
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, grouped_lines, ints, manhattan, neighs, neighs_bounded


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

                neighempt = 0
                neighocc = 0

                for dy in range(max(0, y-1), min(height, y+2)):
                    for dx in range(max(0, x-1), min(width, x+2)):
                        if dy == y and dx == x:
                            continue

                        if lines[dy][dx] == 'L':
                            neighempt += 1
                        elif lines[dy][dx] == '#':
                            neighocc += 1

                if c == 'L' and neighocc == 0:
                    newlines[y][x] = '#'
                    changed = True
                if c == '#' and neighocc >= 4:
                    newlines[y][x] = 'L'
                    changed = True

        if not changed:
            break

        # print(sum(lines[y][x] != newlines[y][x] for x in range(width) for y in range(height) ))
        print('\n'.join(''.join(line) for line in lines))
        print()
        lines = newlines

    return sum(sum(c == '#' for c in row) for row in lines)


if __name__ == '__main__':
    lines = []

    with open('11.txt') as f:
        for line in f.readlines():
            lines.append(line.rstrip())

    print(solve(lines))
