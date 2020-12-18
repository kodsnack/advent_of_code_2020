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

def evaluate(s):
   level = 0
   sub = ""
   op = "+"
   left = 0
   for c in s:
      if c == " ":
         continue
      if c == "(":
         if level > 0:
            sub += c
         level += 1
         continue
      if c == ")":
         level -= 1
         if level == 0:
            right = evaluate(sub)
            sub = ""
         else:
            sub += c
            continue
      elif level > 0:
         sub = sub + c
         continue
      if c in "0123456789":
         right = int(c)
      if c in "+-*":
         op = c
         continue
      if op == "+":
         left += right
      if op == "*":
         left *= right
   return left

def advanced(s):
   print("advanced", s)
   level = 0
   sub = ""
   left = 0
   op = "+"
   stack = list()
   for c in s:
      if c == " ":
         continue
      if c == "(":
         if level > 0:
            sub += c
         level += 1
         continue
      if c == ")":
         level -= 1
         if level == 0:
            right = advanced(sub)
            sub = ""
         else:
            sub += c
            continue
      elif level > 0:
         sub = sub + c
         continue
      if c in "0123456789":
         right = int(c)
      if c in "+-*":
         op = c
         continue
      if op == "*":
         stack.append(left)
         left = right
      elif op == "+":
         left += right
   stack.append(left)
   print(stack)
   p = 1
   for f in stack:
      p *= f
   print("return", p)
   return p

@measure
def part1(pinp):
   return sum(evaluate(s) for s in pinp)

@measure
def part2(pinp):
   return sum(advanced(s) for s in pinp)

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
