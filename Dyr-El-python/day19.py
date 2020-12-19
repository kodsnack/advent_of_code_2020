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

def generateRules(pinp):
   d = {}
   for s in pinp:
      if len(s) == 0:
         return d
      ruleNr = int(s[0][:-1])
      if s[1][0] == '"':
         d[ruleNr] = s[1][1]
      else:
         d[ruleNr] = list()
         seq = []
         for token in s[1:]:
            if token != "|":
               seq.append(int(token))
            else:
               d[ruleNr].append(seq)
               seq = []
         d[ruleNr].append(seq)

def generateLines(pinp):
   inp = iter(pinp)
   while True:
      if len(next(inp)) == 0:
         break
   for line in inp:
      yield line

from collections import deque

def checkLine(s, d, node):
   rules = d[node]
   if isinstance(rules, str) and rules in "ab":
      if len(s) == 0 or s[0] != rules:
         return set()
      else:
         return {1}
   else:
      allMatchLengths = set()
      for rule in rules:
         matchLengths = {0}
         nextMatchLengths = set()
         for part in rule:
            for ml in matchLengths:
               partMatch = checkLine(s[ml:], d, part)
               nextMatchLengths.update(x+ml for x in partMatch)
            matchLengths = nextMatchLengths
            nextMatchLengths = set()
         allMatchLengths.update(matchLengths)
      return allMatchLengths

@measure
def part1(pinp):
   sm = 0
   d = generateRules(pinp)
   for line in generateLines(pinp):
      if len(line) in checkLine(line, d, 0):
         sm += 1
   return sm

@measure
def part2(pinp):
   sm = 0
   d = generateRules(pinp)
   d[8] = [[42], [42, 8]]
   d[11] = [[42, 31], [42, 11, 31]]
   for line in generateLines(pinp):
      if len(line) in checkLine(line, d, 0):
         sm += 1
   return sm

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
