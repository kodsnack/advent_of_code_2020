#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-23

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

class Node:
    def __init__(self, value):
        self.value = value
        self.next = None
        self.prev = None

def parse_input(input):
    return [int(i) for i in input.strip()]

def cups_as_str(c, num):
    s = []
    while num > 0 and c != None:
        s.append(str(c.value))
        num -= 1
        c = c.next
    return " ".join(s)

def play(cups, moves, verbose=False):
    num_cups = len(cups)
    num_to_pick_up = 3
    min_value = min(cups)
    max_value = max(cups)

    cups_map = {}
    for c in cups:
        cups_map[c] = Node(c)
    prev = cups_map[cups[-1]]
    for c in cups_map.values():
        prev.next = c
        c.prev = prev
        prev = c
    prev.next = cups_map[cups[0]]
    current = cups_map[cups[0]]

    for move in range(moves):
        if not verbose and (move+1) % 1000000 == 0:
            print(f"Move {move+1} of {moves}")
        if verbose:
            print(f"-- move {move+1} --")
            print(cups_as_str(current, num_cups))

        # The crab picks up the three cups that are immediately clockwise of the
        # current cup. They are removed from the circle; cup spacing is adjusted
        # as necessary to maintain the circle.
        pick_up = current.next
        next_after_pick_up = pick_up
        for i in range(num_to_pick_up):
            next_after_pick_up = next_after_pick_up.next
        last_pick_up = next_after_pick_up.prev
        last_pick_up.next = None
        current.next = next_after_pick_up
        next_after_pick_up.prev = current

        pick_up_set = set()
        c = pick_up
        while c is not None:
            pick_up_set.add(c.value)
            c = c.next

        if verbose:
            print("pick up:", cups_as_str(pick_up, num_to_pick_up))

        # The crab selects a destination cup: the cup with a label equal to the
        # current cup's label minus one. If this would select one of the cups
        # that was just picked up, the crab will keep subtracting one until it
        # finds a cup that wasn't just picked up. If at any point in this
        # process the value goes below the lowest value on any cup's label, it
        # wraps around to the highest value on any cup's label instead.
        destination_value = current.value - 1
        if destination_value < min_value:
            destination_value = max_value
        while destination_value in pick_up_set:
            destination_value -= 1
            if destination_value < min_value:
                destination_value = max_value
        if verbose:
            print("destination", destination_value)

        # The crab places the cups it just picked up so that they are
        # immediately clockwise of the destination cup. They keep the same order
        # as when they were picked up.
        destination = cups_map[destination_value]
        last_pick_up.next = destination.next
        last_pick_up.next.prev = last_pick_up
        destination.next = pick_up
        pick_up.prev = destination

        # The crab selects a new current cup: the cup which is immediately
        # clockwise of the current cup.
        current = current.next
        if verbose:
            print()
    return cups_map[1]

def solve1(cups, moves):
    first = play(cups, moves, verbose=True)
    res = []
    c = first.next
    while c != first:
        res.append(str(c.value))
        c = c.next
    return "".join(res)

def solve2(cups, moves):
    num = len(cups)
    for i in range(1000000 - num):
        cups.append(num + 1 + i)
    first = play(cups, moves, verbose=False)
    return first.next.value * first.next.next.value

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """389125467
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input), 10), "92658374")
        self.assertEqual(solve1(parse_input(self.input), 100), "67384529")
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input), 10000000), 149245887792)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(solve1(entries, 100))
    print(solve2(entries, 10000000))
