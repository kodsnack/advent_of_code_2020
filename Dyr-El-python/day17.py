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

_cache = dict()
def generateDeltas(dim):
   if dim in _cache:
      for l in _cache[dim]:
         yield l
   else:
      theList = list()
      for i in range(3**dim):
         l = list()
         for j in range(dim):
            l.append(i%3-1)
            i //= 3
         if l.count(0) != dim:
            yield l
            theList.append(l)
      _cache[dim] = theList

def parse(pinp, dim):
   return set(tuple([x, y]+[0]*(dim-2))
              for y, line in enumerate(pinp)
              for x, c in enumerate(line))

from collections import Counter

def runSimultation(pinp, steps, dimensions):
   matrix = parse(pinp, dimensions)
   for i in range(steps):
      neighbours = Counter(tuple(x+d for x, d in zip(coord, delta)) 
                           for delta in generateDeltas(dimensions)
                           for coord in matrix)
      matrix = set(coord for coord, value in neighbours.items()
                   if (coord in matrix and value == 2) or value == 3)
   return len(matrix)

@measure
def part1(pinp):
   return runSimultation(pinp, 6, 3)

@measure
def part2(pinp):
   return runSimultation(pinp, 6, 4)

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
#    inp = """.#.
# ..#
# ###"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
