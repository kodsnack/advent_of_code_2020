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

def takeSteps(s):
   x, y = 0, 0
   div = 1
   for c in s:
      if c == "e":
         x += 2 // div
         div = 1
      elif c == "w":
         x -= 2 // div
         div = 1
      elif c == "s":
         y -= 1
         div = 2
      elif c == "n":
         y += 1
         div = 2
   return x, y

def initialTile(pinp):
   blacks = set()
   for st in pinp:
      x, y = takeSteps(st)
      if (x,y) not in blacks:
         blacks.add((x,y))
      else:
         blacks.remove((x, y))
   return blacks

@measure
def part1(pinp):
   return len(initialTile(pinp))

def printTiles(blacks, candidates):
   mnx = min(x for x, y in candidates)
   mxx = max(x for x, y in candidates)+1
   mny = min(y for x, y in candidates)-1
   mxy = max(y for x, y in candidates)
   for y in range(mxy, mny, -1):
      for x in range(mnx, mxx):
         if (x, y) in blacks:
            print("X", end="")
         elif (x, y) in candidates:
            print(".", end="")
         else:
            print(" ", end="")
      print()


neigbours = [(2, 0), (-2, 0), (1, -1), (1, 1), (-1, -1), (-1, 1)]
def doTileFlips(blacks):
   nextBlacks = set()
   c = set((x+nx, y+ny) for x, y in blacks for nx, ny in neigbours)
   # printTiles(s, c)
   for x, y in c:
      n = sum((x+nx, y+ny) in blacks for nx, ny in neigbours)
      if n == 2 or ((x, y) in blacks and n == 1):
         nextBlacks.add((x, y))
   return nextBlacks

@measure
def part2(pinp):
   blacks = initialTile(pinp)
   for step in range(0, 100):
      blacks = doTileFlips(blacks)
   return len(blacks)

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
#    inp = """"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
