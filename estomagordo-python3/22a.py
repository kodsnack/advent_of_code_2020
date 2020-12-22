
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded


def solve(player1, player2):
    while player1 and player2:
        a = player1[0]
        b = player2[0]

        if a > b:
            player1 = player1[1:] + [a,b]
            player2 = player2[1:]
        else:
            player2 = player2[1:] + [b,a]
            player1 = player1[1:]

    winner = player1 if player1 else player2

    score = 0
    val = 1

    for num in winner[::-1]:
        score += num * val
        val += 1

    return score

if __name__ == '__main__':
    player1 = []
    player2 = []
    broken = False

    with open('22.txt') as f:
        for line in f.readlines():
            if not line.rstrip():
                broken = True
            elif broken and line[0].isdigit():
                player2.append(int(line))
            elif line[0].isdigit():
                player1.append(int(line))

    print(solve(player1, player2))
