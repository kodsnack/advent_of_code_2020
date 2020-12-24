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

from collections import deque

class Layout:
   @staticmethod
   def calcAllKeys(allTiles):
      keys = dict()
      for tv in allTiles:
         for direction in "NSEW":
            if tv._keys[direction] not in keys:
               keys[tv._keys[direction]] = list()
            keys[tv._keys[direction]].append(tv)
      return keys
   def mapTiles(self, startTile, keys):
      self._startTile = startTile
      remaining = deque([self._startTile])
      while remaining:
         current = remaining.popleft()
         if len(current._neighbours) > 0:
            continue
         for direction, key in current._keys.items():
            for candidate in keys[key]:
               if candidate._tileId == current._tileId:
                  continue
               oppositeDir = {"N":"S", "E":"W", "S":"N", "W":"E"}[direction]
               if candidate._keys[oppositeDir] != key:
                  continue
               current._neighbours[direction] = candidate
               remaining.append(candidate)
      self._upperLeft = self._startTile
      while "N" in self._upperLeft._neighbours:
         self._upperLeft = self._upperLeft._neighbours["N"]
      while "W" in self._upperLeft._neighbours:
         self._upperLeft = self._upperLeft._neighbours["W"]
      startOfLine = self._upperLeft
      y = 0
      maxX, maxY = 0, 0
      self._layout = dict()
      while True:
         x = 0
         current = startOfLine
         while True:
            self._layout[x, y] = current
            if "E" in current._neighbours:
               current = current._neighbours["E"]
               x += 1
               maxX = max(x, maxX)
            else:
               break
         if "S" in startOfLine._neighbours:
            startOfLine = startOfLine._neighbours["S"]
            y += 1
            maxY = max(y, maxY)
         else:
            break
      self._maxX, self._maxY = maxX, maxY

   def __init__(self, startTile, allTiles):
      keys = Layout.calcAllKeys(allTiles)
      self.mapTiles(startTile, keys)
   
   def pixel(self, x, y):
      tilex, tiley = x//8, y//8
      subx, suby = x%8, y%8
      if (tilex, tiley) not in self._layout:
         return False
      return self._layout[tilex, tiley]._tile[subx, suby] != "."
   
   def setPixel(self, x, y):
      tilex, tiley = x//8, y//8
      subx, suby = x%8, y%8
      tile = self._layout[tilex, tiley]
      tile._tile[subx, suby] = "O"

   def __str__(self):
      l = list()
      for tileY in range(self._maxY+1):
         for dy in range(8):
            line = list()
            for tileX in range(self._maxX+1):
               for dx in range(8):
                  line.append(self._layout[(tileX, tileY)]._tile[dx, dy])
            l.append(''.join(line))
      return '\n'.join(l)

   def find(self, pattern):
      numberFound = 0
      for x in range(8*self._maxX):
         for y in range(8*self._maxY):
            found = True
            for (dx, dy) in pattern:
               if not self.pixel(x+dx, y+dy):
                  found = False
                  break
            if found:
               numberFound += 1
               for (dx, dy) in pattern:
                  self.setPixel(x+dx, y+dy)
      return numberFound
               

class TileVariant:

   @staticmethod
   def parseTiles(pinp):
      tileStr = list()
      for line in pinp:
         if line == "":
            for flip in (False, True):
               for rotationSteps in range(4):
                  tv = TileVariant(tileStr, rotationSteps, flip)
                  yield tv
            tileStr = list()
         else:
            tileStr.append(line)
      if len(tileStr) > 0:
         for flip in (False, True):
            for rotationSteps in range(4):
               tv = TileVariant(tileStr, rotationSteps, flip)
               yield tv

   def parse(self, s):
      d = dict()
      y = 0
      for line in s:
         if line.startswith("Tile"):
            self._tileId = int(line[5:-1])
            continue
         for x, c in enumerate(line):
            d[(x, y)] = c
         y += 1
      self._tile = {(x-1, y-1):d[x, y] for x in range(1, 9) for y in range(1, 9)}
      self._border = [[d[x, 0] for x in range(10)], [d[9, y] for y in range(10)],
                      [d[9-x, 9] for x in range(10)], [d[0, 9-y] for y in range(10)]]

   def flip(self):
      self._tile = {(7-x, y):c for (x, y), c in self._tile.items()}
      for i in range(4):
         self._border[i].reverse()
      self._border = [self._border[0], self._border[3], self._border[2], self._border[1]]

   def rotate(self, steps):
      for i in range(steps):
         self._tile = {(y, 7-x):c for (x, y), c in self._tile.items()}
         self._border = self._border[1:] + self._border[:1]

   def calcKeys(self):
      self._keys = {"N":"".join(self._border[0]),
         "S":"".join(reversed(self._border[2])),
         "E":"".join(self._border[1]),
         "W":"".join(reversed(self._border[3]))}

   def __init__(self, s, rotation, flip):
      self.parse(s)
      if flip:
         self.flip()
      self.rotate(rotation)
      self.calcKeys()
      self._neighbours = dict()
   
   def __str__(self):
      l = [f"Tile {self._tileId}", " Borders:"]
      for border in self._border:
         l.append(f"  [{border}]")
      l.append(" Layout:")
      for y in range(8):
         line = ["  '"]
         for x in range(8):
            line.append(self._tile[x, y])
         l.append(''.join(line) + "'")
      l.append(" ".join(d+":"+k for d, k in self._keys.items()))
      return '\n'.join(l)

def seaMonster():
   st = """                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """
   s = set()
   for y, line in enumerate(st.splitlines()):
      for x, c in enumerate(line):
         if c == "#":
            s.add((x, y))
   return s

from functools import reduce
from operator import mul

@measure
def part1(pinp):
   tileVariants = list(TileVariant.parseTiles(pinp))
   layouts = [Layout(tv, tileVariants) for tv in tileVariants[:1]]
   corners = list()
   for i in range(1):
      mxX, mxY = layouts[i]._maxX, layouts[i]._maxY
      for x in (0, mxX):
         for y in (0, mxY):
            corners.append(layouts[i]._layout[x,y]._tileId)
   return reduce(mul, corners)

@measure
def part2(pinp):
   tileVariants = list(TileVariant.parseTiles(pinp))
   layouts = [Layout(tv, tileVariants) for tv in tileVariants[:8]]
   for layout in layouts:
      number = layout.find(seaMonster())
      if number > 0:
         return str(layout).count("#")
   return "Not found"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   # inp = """"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=oneLinePattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
