#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-14

import math
import os
import re
import unittest
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    return input.strip().split("\n")

def parse_bin_str(bin_str):
    mask_set = 0
    mask_reset = 0
    for i,s in enumerate(bin_str):
        mask_set <<= 1
        mask_reset <<= 1
        if s == "0":
            mask_reset |= 1
        elif s == "1":
            mask_set |= 1
    return mask_set, mask_reset

def get_floating_bits(bin_str):
    floating_bits = set()
    bits = len(bin_str)
    for i,s in enumerate(bin_str):
        if s == "X":
            floating_bits.add(bits-i-1)
    return floating_bits

def solve1(entries):
    memory = {}
    mask = None
    mask_set = None
    mask_reset = None
    for e in entries:
        m1 = re.match(r"mask = ([01X]+)", e)
        m2 = re.match(r"mem\[(\d+)] = (\d+)", e)
        if m1:
            mask = m1.group(1)
            mask_set, mask_reset = parse_bin_str(m1.group(1))
        elif m2:
            address, value = int(m2.group(1)), int(m2.group(2))
            value |= mask_set
            value &= ~mask_reset
            memory[address] = value
        else:
            raise ValueError("unknown instruction " + e)
    s = 0
    for m in memory.values():
        s += m
    return sum(memory.values())

def floating_addresses(address, floating_bits):
    res = []
    bits = []
    for i,bit in enumerate(floating_bits):
        bits.append(bit)
    for i in range(2 ** len(floating_bits)):
        a = address
        for bi,b in enumerate(bits):
            if i & (1 << bi):
                a |= (1 << b)
            else:
                a &= ~(1 << b)
        res.append(a)
    return res

def solve2(entries):
    # If the bitmask bit is 0, the corresponding memory address bit is unchanged.
    # If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
    # If the bitmask bit is X, the corresponding memory address bit is floating.
    memory = {}
    mask = None
    mask_set = None
    mask_reset = None
    floating_bits = None
    for e in entries:
        m1 = re.match(r"mask = ([01X]+)", e)
        m2 = re.match(r"mem\[(\d+)] = (\d+)", e)
        if m1:
            mask = m1.group(1)
            mask_set, mask_reset = parse_bin_str(m1.group(1))
            floating_bits = get_floating_bits(m1.group(1))
        elif m2:
            address, value = int(m2.group(1)), int(m2.group(2))
            address |= mask_set
            for f in floating_addresses(address, floating_bits):
                memory[f] = value
        else:
            raise ValueError("unknown instruction " + e)
    s = 0
    return sum(memory.values())

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"""
    input2 = """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 165)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input2)), 208)

if __name__ == "__main__":
    problem_name = os.path.splitext(os.path.basename(__file__))[0]
    with open(f"input/{problem_name}.txt") as f:
        entries = parse_input(f.read())
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
