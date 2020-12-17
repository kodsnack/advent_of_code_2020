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

def generateDeltas(dim):
   for i in range(3**dim):
      l = list()
      for j in range(dim):
         l.append(i%3-1)
         i //= 3
      if l.count(0) != dim:
         yield l

def parse(pinp, dim):
   d = set()
   for y, line in enumerate(pinp):
      for x, c in enumerate(line):
         if c == "#":
            d.add(tuple([x, y]+[0]*(dim-2)))
   return d

@measure
def part1(pinp):
   matrix = parse(pinp, 3)
   for i in range(6):
      s = set()
      for (x, y, z) in matrix:
         for (dx, dy, dz) in generateDeltas(3):
            s.add((x+dx, y+dy, z+dz))
      nextMatrix = set()
      for (x, y, z) in s:
         neighbours = 0
         for (dx, dy, dz) in generateDeltas(3):
            if (x+dx, y+dy, z+dz) in matrix:
               neighbours += 1
         if (x, y, z) in matrix:
            if neighbours in (2,3):
               nextMatrix.add((x, y, z))
         else:
            if neighbours == 3:
               nextMatrix.add((x, y, z))
      matrix = nextMatrix
   return len(matrix)

@measure
def part2(pinp):
   matrix = parse(pinp, 4)
   for i in range(6):
      s = set()
      for (x, y, z, w) in matrix:
         for (dx, dy, dz, dw) in generateDeltas(4):
            s.add((x+dx, y+dy, z+dz, w+dw))
      nextMatrix = set()
      for (x, y, z, w) in s:
         neighbours = 0
         for (dx, dy, dz, dw) in generateDeltas(4):
            if (x+dx, y+dy, z+dz, w+dw) in matrix:
               neighbours += 1
         if (x, y, z, w) in matrix:
            if neighbours in (2,3):
               nextMatrix.add((x, y, z, w))
         else:
            if neighbours == 3:
               nextMatrix.add((x, y, z, w))
      matrix = nextMatrix
   return len(matrix)

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
