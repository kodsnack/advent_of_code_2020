#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-16
# A quite hacky solution

import math
import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return input.strip().split("\n")

def split_input(entries):
    rules = {}
    line = 0
    while entries[line]:
        e = entries[line]
        rule_name, ranges = e.split(":")
        rule_ranges = []
        for r in ranges.split("or"):
            rule_ranges.append([int(s) for s in r.strip().split("-")])
        rules[rule_name] = rule_ranges
        line += 1
    line += 1
    assert entries[line] == "your ticket:"
    line += 1
    mine = [int(s) for s in entries[line].split(",")]
    line += 2
    assert entries[line] == "nearby tickets:"
    line += 1
    nearby = []
    while line < len(entries):
        nearby.append([int(s) for s in entries[line].split(",")])
        line += 1
    return rules, mine, nearby

def is_valid_value(value, rules):
    for rule in rules.values():
        for r in rule:
            if value >= r[0] and value <= r[1]:
                return True
    return False

def solve1(entries):
    rules, mine, nearby = split_input(entries)
    invalid_sum = 0
    for e in nearby:
        for v in e:
            if not is_valid_value(v, rules):
                invalid_sum += v
    return invalid_sum

def is_valid_value_according_to_rule(value, rule):
    for r in rule:
        if value >= r[0] and value <= r[1]:
            return True
    return False

def is_valid_ticket(ticket, rules):
    for v in ticket:
        if not is_valid_value(v, rules):
            return False
    return True

def solve2(entries, rules_to_multiply):
    rules, mine, nearby = split_input(entries)
    valid_tickets = []
    for e in nearby:
        if is_valid_ticket(e, rules):
            valid_tickets.append(e)
    fields = []
    for i in range(len(rules)):
        possible_rules = set()
        for r, rule in rules.items():
            possible = True
            for t in valid_tickets:
                if not is_valid_value_according_to_rule(t[i], rule):
                    possible = False
                    break
            if possible:
                possible_rules.add(r)
        fields.append(possible_rules)
    applied_rules = set()
    while True:
        remove_rule = None
        for index,f in enumerate(fields):
            first_rule = next(iter(f))
            if len(f) == 1 and first_rule not in applied_rules:
                remove_rule = first_rule
                field_index = index
                break
        if remove_rule is None:
            break
        applied_rules.add(remove_rule)
        for i,f in enumerate(fields):
            if i == field_index:
                continue
            if remove_rule in f:
                f.remove(remove_rule)
    for f in fields:
        assert len(f) == 1
    res = 1
    for i,value in enumerate(mine):
        rule = next(iter(fields[i]))
        if rule.startswith(rules_to_multiply):
            res *= value
    return res

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
"""
    input2 = """class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 71)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input2), "class"), 12)
        self.assertEqual(solve2(parse_input(self.input2), "row"), 11)
        self.assertEqual(solve2(parse_input(self.input2), "seat"), 13)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(solve1(entries))
    print(solve2(entries, "departure"))
