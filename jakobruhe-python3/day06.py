#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-06
# 06:04: started
# 06:23: p1 solved
# 06:43: p2 solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return input.strip().split("\n");

def split_in_groups(entries):
    groups = []
    group = []
    for e in entries:
        if e:
            group.append(e)
        elif group:
            groups.append(group)
            group = []
    if group:
        groups.append(group)
    return groups

def num_unique(participants):
    yes_questions = set()
    for p in participants:
        for s in p:
            yes_questions.add(s)
    return len(yes_questions)

def solve1(entries):
    groups = split_in_groups(entries)
    s = 0
    for g in groups:
        s = s + num_unique(g)
    return s

def get_questions(groups):
    questions = set()
    for g in groups:
        for p in g:
            for q in p:
                questions.add(q)
    return questions

def calc_num_all_yes(participants, questions):
    answers = {}
    for q in questions:
        answers[q] = 0
    for p in participants:
        for a in p:
            answers[a] += 1
    num_all = 0
    for a in answers.values():
        if a == len(participants):
            num_all += 1
    return num_all

def solve2(entries):
    groups = split_in_groups(entries)
    questions = get_questions(groups)
    s = 0
    for g in groups:
        s = s + calc_num_all_yes(g, questions)
    return s

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """abc

a
b
c

ab
ac

a
a
a
a

b
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 11)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 6)

if __name__ == "__main__":
    with open("input/day06.txt") as f:
        input = f.read()
    entries = parse_input(input)
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
