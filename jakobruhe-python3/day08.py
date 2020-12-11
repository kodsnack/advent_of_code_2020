#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-08
# 06:10: started
# 06:25: solved
# 06:54: solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

Instruction = namedtuple('Instruction', ['op', 'arg'])

def parse_input(input):
    lines = input.strip().split("\n");
    instructions = []
    for line in lines:
        m = re.match(r"(\w+) ([+\-]\d+)", line)
        instructions.append(Instruction(m.group(1), int(m.group(2))))
    return instructions

def run(prog):
    history = set()
    pc = 0
    acc = 0
    while True:
        instr = prog[pc]
        #print(f"pc {pc} acc {acc}: {instr.op} {instr.arg}")
        if instr.op == "acc":
            acc += instr.arg
            pc += 1
        elif instr.op == "jmp":
            pc += instr.arg
        elif instr.op == "nop":
            pc += 1
        else:
            raise ValueError(f"Unknown instruction: {instr.op}")
        if pc >= len(prog):
            #print(f"End of program, pc: {pc}, stopping")
            break
        elif pc in history:
            #print(f"Already been at {pc}, stopping")
            break
        history.add(pc)
    return (pc, acc)

def solve1(prog):
    pc, acc = run(prog)
    return acc

def modify(prog, mod_pos):
    mod = prog[:]
    mod_pos += 1
    while mod[mod_pos].op == "acc":
        mod_pos += 1
    old_instr = mod[mod_pos]
    if old_instr.op == "nop":
        mod[mod_pos] = Instruction("jmp", old_instr.arg)
    elif old_instr.op == "jmp":
        mod[mod_pos] = Instruction("nop", old_instr.arg)
    return (mod, mod_pos)

def solve2(prog):
    mod_pos = -1
    while True:
        mod, mod_pos = modify(prog, mod_pos)
        pc, acc = run(mod)
        if pc >= len(mod):
            break
    return acc

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 5)
    def test2(self):
        self.assertEqual(solve2(parse_input(self.input)), 8)

if __name__ == "__main__":
    with open("input/day08.txt") as f:
        input = f.read()
    entries = parse_input(input)
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
