
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded


def game(gameseen, player1, player2):
    while player1 and player2:
        t = (tuple(player1), tuple(player2))

        if t in gameseen:
            return (player1, player2)

        gameseen.add(t)

        a = player1[0]
        b = player2[0]
        player1 = player1[1:]
        player2 = player2[1:]

        if a > len(player1) or b > len(player2):
            if a > b:
                player1 += [a,b]
            else:
                player2 += [b,a]
            continue            

        recplay1, _ = game(set(), player1[:a], player2[:b])

        if recplay1:
            player1 += [a,b]
        else:
            player2 += [b,a]
        
    return (player1, player2)


def solve(player1, player2):
    player1, player2 = game(set(), player1, player2)
    winner = player1 if player1 else player2

    return sum(winner[x] * (len(winner)-x) for x in range(len(winner)))

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

# 32618 30843