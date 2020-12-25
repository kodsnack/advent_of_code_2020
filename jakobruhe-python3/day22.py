#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-22
# 06:18: p1 solved
# 07:31: p2 solved
#
# Got the cache wrong... took way to long to figure out.

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple


def parse_input(input):
    lines = input.strip().split("\n")
    line_p2 = lines.index("Player 2:")
    p1 = tuple([int(e) for e in lines[1:line_p2-1]])
    p2 = tuple([int(e) for e in lines[line_p2+1:]])
    return p1, p2


def play(recursive_combat, game, p1, p2, verbose=False):
    step = 0
    mem = set()
    subgame = game + 1
    while p1 and p2:
        step += 1
        if verbose:
            print(f"-- Round {step} (Game {game}) --")
            print("p1", p1)
            print("p2", p2)
        if recursive_combat and hash((p1, p2)) in mem:
            if verbose:
                print("Seen before so p1 wins this subgame!")
            return (1, p1)
        mem.add(hash((p1,p2)))
        c1, c2 = p1[0], p2[0]
        p1, p2 = p1[1:], p2[1:]
        winner_of_round = None
        if recursive_combat and len(p1) >= c1 and len(p2) >= c2:
            winner_of_round, _ = play(recursive_combat, subgame, p1[:c1], p2[:c2])
            subgame += 1
        else:
            winner_of_round = 1 if c1 > c2 else 2
        if winner_of_round == 1:
            p1 = p1 + (c1, c2)
        else:
            p2 = p2 + (c2, c1)
    return (1, p1) if p1 else (2, p2)


def calculate_score(deck):
    return sum([c*(i+1) for i,c in enumerate(reversed(deck))])


def solve1(p1, p2, verbose=False):
    winner, deck = play(False, 1, p1, p2, verbose=verbose)
    print(f"Player {winner} won with deck: {deck}")
    return calculate_score(deck)


def solve2(p1, p2, verbose=False):
    winner, deck = play(True, 1, p1, p2, verbose=verbose)
    print(f"Player {winner} won with deck: {deck}")
    return calculate_score(deck)


# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
"""
    def test1(self):
        self.assertEqual(solve1(*parse_input(self.input), verbose=True), 306)
    def test2(self):
        self.assertEqual(solve2(*parse_input(self.input), verbose=True), 291)


if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        p1, p2 = parse_input(f.read())
    print(solve1(p1, p2))
    print(solve2(p1, p2))
