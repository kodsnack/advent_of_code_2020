#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-21

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    lines = input.strip().split("\n")
    food_list = []
    for line in lines:
        ingrediens_str, allergens_str = line.split(" (contains ")
        food_list.append((set(ingrediens_str.split()),
                          set(allergens_str.replace(")", "").split(", "))))
    return food_list

def find_allergene_not_including(foods, allergene, ingrediens):
    for f in foods:
        if ingrediens not in f[0] and allergene in f[1]:
            return f
    return None

def find_initial_possible_allergenes(foods):
    possible_allergenes = defaultdict(set)
    for f in foods:
        for i in f[0]:
            for a in f[1]:
                possible_allergenes[i].add(a)
    return possible_allergenes

def remove_impossible_allergenes1(foods, possible_allergenes):
    for i,p in possible_allergenes.items():
        remove = set()
        for a in p:
            f = find_allergene_not_including(foods, a, i)
            if f is not None:
                print("Removing allergene", a, "from ingrediens", i, "food", f)
                remove.add(a)
        for a in remove:
            p.remove(a)

def print_possible_allergenes(possible_allergenes):
    for i,p in possible_allergenes.items():
        print("-", i, ":", " ".join(p))

def solve1(foods):
    print("foods")
    for f in foods:
        print(f)
    possible_allergenes = find_initial_possible_allergenes(foods)
    print("possible_allergenes")
    print_possible_allergenes(possible_allergenes)
    remove_impossible_allergenes1(foods, possible_allergenes)
    print("possible_allergenes after removal")
    print_possible_allergenes(possible_allergenes)
    res = 0
    for f in foods:
        for a,p in possible_allergenes.items():
            res += not p and a in f[0]
    return res

def remove_impossible_allergenes2(foods, possible_allergenes):
    checked_allergenes = set()
    while True:
        remove_allergene = None
        not_from_ingrediens = None
        for i,p in possible_allergenes.items():
            if not p:
                continue
            first_allergene = next(iter(p))
            if len(p) == 1 and first_allergene not in checked_allergenes:
                remove_allergene = first_allergene
                not_from_ingrediens = i
                break
        if remove_allergene is None:
            break
        for i,p in possible_allergenes.items():
            if i == not_from_ingrediens:
                continue
            if remove_allergene in p:
                print("Removing allergene", remove_allergene, "from", i)
                p.remove(remove_allergene)
        checked_allergenes.add(remove_allergene)

def solve2(foods):
    possible_allergenes = find_initial_possible_allergenes(foods)
    remove_impossible_allergenes1(foods, possible_allergenes)
    remove_impossible_allergenes2(foods, possible_allergenes)
    print("possible_allergenes after removal step 2")
    print_possible_allergenes(possible_allergenes)
    dangerous = {}
    for i,p in possible_allergenes.items():
        assert len(p) <= 1
        if len(p) == 1:
            dangerous[i] = next(iter(p))
    print("dangerous ingredienses")
    for i,a in dangerous.items():
        print("-", i, ":", a)
    sorted_di = dict(sorted(dangerous.items(), key=lambda item: item[1]))
    return ",".join(sorted_di.keys())

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 5)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), "mxmxvkd,sqjhc,fvjkl")

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(solve1(entries))
    print(solve2(entries))
