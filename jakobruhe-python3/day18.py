#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-18

# First day I did not solve P1 early morning.
# But both problems were at least solved on the correct day.
# P1 is solved with a local invented recursive thing.
# For P2 I had to lookup how to do infix to postfix conversion.

import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return input.strip().split("\n")

def tokenize(expr):
    return expr.replace(")", " ) ").replace("(", " (  ").split()

def evaluate(tokens):
    if len(tokens) == 1:
        return int(tokens[0])
    if tokens[-1] == ")":
        depth = 1
        pos = len(tokens) - 1
        while depth > 0:
            pos -= 1
            if tokens[pos] == "(":
                depth -= 1
            elif tokens[pos] == ")":
                depth += 1
        rhs = evaluate(tokens[pos + 1:len(tokens) - 1])
        if pos == 0:
            return rhs
        lhs = evaluate(tokens[0:pos - 1])
        op = tokens[pos - 1]
    else:
        lhs = evaluate(tokens[0:len(tokens)-2])
        op = tokens[-2]
        rhs = int(tokens[-1])
    if op == "+":
        return lhs + rhs
    elif op == "-":
        return lhs - rhs
    elif op == "*":
        return lhs * rhs
    else:
        raise ValueError(f"Unknown op: {op}")

def solve1(entries):
    return sum([evaluate(tokenize(e)) for e in entries])

def pred(tok):
    pred_ = {"+": 2, "-": 2, "*": 1}
    return pred_[tok]

def infix_to_postfix(tokens):
    # Comments and inspiration from:
    # https://www.geeksforgeeks.org/stack-set-2-infix-to-postfix/
    result = []
    stack = []
    for t in tokens:
        if t.isnumeric():
            # If the scanned character is an operand, output it.
            result.append(int(t))
        elif t == "(":
            # If the scanned character is an ‘(‘, push it to the stack.
            stack.append(t)
        elif t == ")":
            # If the scanned character is an ‘)’, pop the stack and output
            # it until a ‘(‘ is encountered, and discard both the parenthesis.
            while stack:
                top = stack.pop()
                if top == "(":
                    break
                result.append(top)
        elif not stack or stack[-1] == "(" or pred(t) > pred(stack[-1]):
            #  If the precedence of the scanned operator is greater than the
            #  precedence of the operator in the stack (or the stack is empty
            #  or the stack contains a ‘(‘ ), push it.
            stack.append(t)
        else:
            #  Else, Pop all the operators from the stack which are greater
            #  than or equal to in precedence than that of the scanned
            #  operator. After doing that Push the scanned operator to the
            #  stack. (If you encounter parenthesis while popping then stop
            #  there and push the scanned operator in the stack.)
            while stack and stack[-1] != "(" and pred(stack[-1]) >= pred(t):
                result.append(stack.pop())
            stack.append(t)
    while stack:
        result.append(stack.pop())
    return result

def eval_postfix(postfix):
    stack = []
    for t in postfix:
        if isinstance(t, int):
            stack.append(t)
            continue
        rhs = stack.pop()
        lhs = stack.pop()
        if t == "+":
            stack.append(lhs + rhs)
        elif t == "-":
            stack.append(lhs - rhs)
        elif t == "*":
            stack.append(lhs * rhs)
        else:
            raise ValueError(f"Unknown token {t}")
    return stack[-1]

def evaluate2(tokens):
    postfix = infix_to_postfix(tokens)
    return eval_postfix(postfix)

def solve2(entries):
    return sum([evaluate2(tokenize(e)) for e in entries])

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """
"""
    def test1(self):
        self.assertEqual(solve1(parse_input("1")), 1)
        self.assertEqual(solve1(parse_input("1 + 2")), 3)
        self.assertEqual(solve1(parse_input("1 + 2 + 3")), 6)
        self.assertEqual(solve1(parse_input("1 + 2 + 3 + 4")), 10)
        self.assertEqual(solve1(parse_input("1 + 2 * 3 + 1")), 10)
        self.assertEqual(solve1(parse_input("1 - (2 + 3)")), -4)

    def test2(self):
        self.assertEqual(solve2(parse_input("1")), 1)
        self.assertEqual(solve2(parse_input("1 + 2")), 3)
        self.assertEqual(solve2(parse_input("1 - 2")), -1)
        self.assertEqual(solve2(parse_input("1 - 2 - 3")), -4)
        self.assertEqual(solve2(parse_input("1 - 2 + 3")), 2)
        self.assertEqual(solve2(parse_input("1 - 2 + 3 + 4 + 5")), 11)
        self.assertEqual(solve2(parse_input("2 - (1 + 3)")), -2)
        self.assertEqual(solve2(parse_input("(1 + 2)")), 3)
        self.assertEqual(solve2(parse_input("((1 + 2))")), 3)
        self.assertEqual(solve2(parse_input("(1 + 2) * (3 + 4)")), 21)
        self.assertEqual(solve2(parse_input("2 * 3 + (4 * 5)")), 46)
        self.assertEqual(solve2(parse_input("(2 * 3) + (4 * 5)")), 26)
        self.assertEqual(solve2(parse_input("1 + 2 * 3 + 1")), 12)
        self.assertEqual(solve2(parse_input("5 + (8 * 3 + 9 + 3 * 4 * 3)")), 1445)
        self.assertEqual(solve2(parse_input("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")), 669060)
        self.assertEqual(solve2(parse_input("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")), 23340)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/day18.txt") as f:
        entries = parse_input(f.read())
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
