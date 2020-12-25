#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-19

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple


def parse_input(input):
    entries = input.split("\n")
    empty_line_at = entries.index("")
    rule_lines = entries[0:empty_line_at]
    rules = {}
    for r in rule_lines:
        rule_id, sub_rules = r.split(":")
        rule = []
        for sr in sub_rules.split("|"):
            patterns = [int(pp) if pp.isnumeric() else pp.replace("\"", "") for pp in sr.split()]
            rule.append(patterns)
        rules[int(rule_id)] = rule
    messages = entries[empty_line_at + 1:]
    return rules, messages


def matches_sub_rule(message, sub_rule, rules):
    if not sub_rule:
        return len(message) == 0
    p = sub_rule[0]
    if isinstance(p, int):
        rule = rules[p]
        for rule_sub_rules in rule:
            if matches_sub_rule(message, rule_sub_rules + sub_rule[1:], rules):
                return True
    elif message.startswith(p):
        return matches_sub_rule(message[len(p):], sub_rule[1:], rules)
    return False


def matches_rule(message, rule_id, rules):
    for sub_rule in rules[rule_id]:
        if matches_sub_rule(message, sub_rule, rules):
            return True
    return False


# How many messages completely match rule 0?
def solve1(rules, messages):
    return sum(matches_rule(m, 0, rules) for m in messages)


def solve2(rules, messages):
    rules[8] = [[42], [42, 8]]
    rules[11] = [[42, 31], [42, 11, 31]]
    return sum(matches_rule(m, 0, rules) for m in messages)


# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
"""
    input2 = """42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"""
    def test1(self):
        self.assertEqual(solve1(*parse_input(self.input)), 2)
    def test2(self):
        self.assertEqual(solve1(*parse_input(self.input2)), 3)
        self.assertEqual(solve2(*parse_input(self.input2)), 12)


if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        rules, messages = parse_input(f.read())
    print(solve1(rules, messages))
    print(solve2(rules, messages))
