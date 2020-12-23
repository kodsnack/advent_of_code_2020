
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded


def solve(labeling, moves):
    successor = {}

    for x, label in enumerate(labeling):
        if x > 0:
            successor[int(labeling[x-1])] = int(label)

    successor[int(labeling[-1])] = 10

    for x in range(10, 10**6):
        successor[x] = x+1

    successor[10**6] = int(labeling[0])

    current = int(labeling[0])

    for _ in range(moves):
        if _ % 100000 == 0:
            print(_)
        a = successor[current]
        b = successor[a]
        c = successor[b]
        d = successor[c]

        successor[current] = d

        destination = current-1
        if destination == 0:
            destination = len(successor)

        if destination > 3:
            while destination in (a, b, c):
                destination -= 1
        elif destination == 3:
            if 3 in (a, b, c):
                if 2 in (a, b, c):
                    if 1 in (a, b, c):
                        destination = len(successor)
                    else:
                        destination = 1
                else:
                    destination = 2
            else:
                destination = 3
        elif destination == 2:
            if 2 in (a, b, c):
                if 1 in (a, b, c):
                    if len(successor) in (a, b, c):
                        destination = len(successor)-1
                    else:
                        destination = len(successor)
                else:
                    destination = 1
            else:
                destination = 2
        elif destination == 1:
            if 1 in (a, b, c):
                if len(successor) in (a, b, c):
                    if len(successor)-1 in (a, b, c):
                        destination = len(successor)-2
                    else:
                        destination = len(successor)-1
                else:
                    destination = len(successor)
            else:
                destination = 1

        successor[c] = successor[destination]
        successor[destination] = a
        current = successor[current]

    print(successor[1], successor[successor[1]])
    return successor[1] * successor[successor[1]]

if __name__ == '__main__':
    print(solve('389547612', 10**7))

# 149245887792 too high