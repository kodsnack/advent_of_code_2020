#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys, re

def parse_rule(rule_string):
    p = re.compile(r'(\d+)-(\d+) (\w)')
    m = p.match(rule_string)
    return((int(m.group(1)), int(m.group(2)), m.group(3)))

def is_password_valid_policy_1(rule_string, password):
    n_min, n_max, letter = parse_rule(rule_string)
    n = password.count(letter)
    return (n >= n_min) and (n <= n_max)

def is_password_valid_policy_2(rule_string, password):
    i_first, i_second, letter = parse_rule(rule_string)
    one_is_true = (password[i_first-1] == letter) != (password[i_second-1] == letter)
    return one_is_true

@timing
def part1(data):
    n_valid_passwords = sum([ 1 for (rule, password) in data if is_password_valid_policy_1(rule, password)])
    return n_valid_passwords

@timing
def part2(data):
    n_valid_passwords = sum([ 1 for (rule, password) in data if is_password_valid_policy_2(rule, password)])
    return n_valid_passwords

## Unit tests ########################################################

class TestDay02(unittest.TestCase):
    def test_parse_rule_1(self):
        self.assertEqual(parse_rule("1-3 a"),(1, 3, 'a'))

    def test_parse_rule_2(self):
        self.assertEqual(parse_rule("1-3 b"),(1, 3, 'b'))

    def test_parse_rule_3(self):
        self.assertEqual(parse_rule("2-9 c"),(2, 9, 'c'))

    def test_policy_1_is_password_valid_1(self):
        self.assertEqual(is_password_valid_policy_1("1-3 a","abcde"), True)

    def test_policy_1_is_password_valid_2(self):
        self.assertEqual(is_password_valid_policy_1("1-3 b","cdefg"), False)

    def test_policy_1_is_password_valid_3(self):
        self.assertEqual(is_password_valid_policy_1("2-9 c","ccccccccc"), True)

    def test_policy_2_is_password_valid_1(self):
        self.assertEqual(is_password_valid_policy_2("1-3 a","abcde"), True)

    def test_policy_2_is_password_valid_2(self):
        self.assertEqual(is_password_valid_policy_2("1-3 b","cdefg"), False)

    def test_policy_2_is_password_valid_3(self):
        self.assertEqual(is_password_valid_policy_2("2-9 c","ccccccccc"), False)

    def test_part1(self):
        self.assertEqual(part1([("1-3 a", "abcde"),
                                ("1-3 b", "cdefg"),
                                ("2-9 c", "ccccccccc")]), 2)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day 2")
    print("Part1 result: {}".format(part1(getTuplesFromFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(getTuplesFromFile(sys.argv[1]))))
