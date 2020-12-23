
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

    n = len(successor)

    current = int(labeling[0])

    for _ in range(moves):
        a = successor[current]
        b = successor[a]
        c = successor[b]
        d = successor[c]

        moving = [a,b,c]

        successor[current] = d

        destination = ((current-2) % n) +1
        
        while destination in moving:
            destination = ((destination-2) % n) +1
            
        successor[c] = successor[destination]
        successor[destination] = a
        current = successor[current]
        
    return successor[1] * successor[successor[1]]

if __name__ == '__main__':
    print(solve('389547612', 10**7))