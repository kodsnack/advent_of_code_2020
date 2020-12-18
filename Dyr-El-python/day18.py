## Start of header boilerplate #################################################

from aocbase import readInput, measure
import re

oneLinePattern = re.compile(r"^.*$")
wsTokenPattern = re.compile(r"\S+")

def lineParse(s, f, fp):
   m = re.findall(fp, s)
   if m==None:
      raise ValueError("No token patterns found")
   return tuple(map(f, m))

def fileParse(inp, lineparser=lineParse,
                   tokenparser=lambda x:x,
                   tokenPattern=re.compile(r"^(.*)$")):
   return tuple(map(lambda x:x[0] if len(x)==1 else x, 
                map(lambda x:lineparser(x, tokenparser, tokenPattern),
                    inp.splitlines())))

## End of header boilerplate ###################################################

from operator import add, mul
from collections import deque

def lexer(s):
   ops = {"+":add, "*":mul}
   for c in s:
      if c in "0123456789":
         yield int(c)
      elif c in ops:
         yield ops[c]
      elif c in "()":
         yield c

def simple(tokens):
   memory = deque()
   for token in tokens:
      if token == "(":
         memory.append(simple(tokens))
      elif token == ")":
         break
      else:
         memory.append(token)
      if len(memory) > 2:
         left, op, right = memory.popleft(), memory.popleft(), memory.popleft()
         memory.append(op(left, right))
   return memory.popleft()

def advanced(tokens):
   memory = deque()
   for token in tokens:
      if token == "(":
         memory.append(advanced(tokens))
      elif token == ")":
         break
      else:
         memory.append(token)
      if len(memory) > 2:
         if memory[-2] == add:
            right, op, left = memory.pop(), memory.pop(), memory.pop()
            memory.append(op(left, right))
   while len(memory) > 1:
      left, op, right = memory.popleft(), memory.popleft(), memory.popleft()
      memory.appendleft(op(left, right))
   return memory.pop()

@measure
def part1(pinp):
   return sum(simple(lexer(s)) for s in pinp)

@measure
def part2(pinp):
   return sum(advanced(lexer(s)) for s in pinp)

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   inp2 = """2 * 3 + (4 * 5)
5 + (8 * 3 + 9 + 3 * 4 * 3)
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=oneLinePattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
