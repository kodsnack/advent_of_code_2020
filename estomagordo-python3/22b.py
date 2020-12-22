
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product
import re
from sys import setrecursionlimit

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded

setrecursionlimit(8000)

def player1_wins_round(gameseen, player1, player2):
    if not player1:
        return False
    if not player2:
        return True
    
    if tuple(player1 + player2) in gameseen:
        return True

    gameseen.add(tuple(player1 + player2))

    a = player1[0]
    b = player2[0]

    player1 = player1[1:]
    player2 = player2[1:]

    if a > len(player1) or b > len(player2):
        if a > b:
            player1 += [a,b]
        else:
            player2 += [b,a]

        return player1_wins_round(gameseen, player1, player2)

    awins = player1_wins_round(set(), player1[:a], player2[:b])

    if awins:
        player1 += [a,b]
    else:
        player2 += [b,a]

    return player1_wins_round(gameseen, player1, player2)


def game(player1, player2):
    while player1 and player2:
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

        # awins = player1_wins_round(set(), list(player1[:a]), list(player2[:b]))

        if awins:
            player1 += [a,b]
        else:
            player2 += [b,a]
        
    return player1 if player1 else player2


def solve(player1, player2):
    winner = game(player1, player2)

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

# 32618 30843