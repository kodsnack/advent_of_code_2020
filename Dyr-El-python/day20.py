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

def generateVariants(tile):
   l = [[] for i in range(8)]
   for c in range(10):
      for r in range(10):
         l[0].append(tile[c][r])
         l[1].append(tile[9-c][r])
         l[2].append(tile[9-r][c])
         l[3].append(tile[r][c])
         l[4].append(tile[9-c][9-r])
         l[5].append(tile[c][9-r])
         l[6].append(tile[r][9-c])
         l[7].append(tile[9-r][9-c])
   return l

def parseTiles(pinp):
   d = dict()
   for line in pinp:
      if len(line) == 0:
         continue
      if isinstance(line, tuple):
         tileNo = int(line[1][:-1])
         currentTile = list()
         d[tileNo] = currentTile
         row = 0
         continue
      currentTile.append(list(line))
   d2 = dict()
   for key in d:
      d2[key] = generateVariants(d[key])
   return d2

def pieces(tiles, commonSides):
   keys = dict()
   for key, ts in tiles.items():
      for v in ts:
         k = tuple(v[:10])
         if k not in keys:
            keys[k] = set()
         keys[k].add(key)
   l = list()
   for key, ts in tiles.items():
      sm = 0
      for v in ts:
         k = tuple(v[:10])
         if len(keys[k]) == 1:
            sm +=1
      if sm == (4-commonSides)*2:
         l.append(key)
   return l

def printTile(tile):
   return "\n".join(''.join(tile[x:x+10]) for x in range(0, 100, 10))

def printTiles(tiles):
   return "\n\n".join(printTile(t) for t in tiles)

def printLayot(lt):
   line, column = 0, 0
   if (column, line) not in lt:
      return
   while True:
      print(lt[(column, line)], end="")
      column += 1
      if (column, line) not in lt:
         print()
         column = 0
         line += 1
         if (column, line) not in lt:
            return

def layPuzzle(tiles, lt, c=0, r=0):
   print("lay puzzle", c, r)
   printLayot(lt)
   if r == 12:
      return lt
   if c in (0, 11) and r in (0, 11):
      ps = pieces(tiles, 2)
   elif (c in (0, 11)) != (r in (0, 11)):
      ps = pieces(tiles, 3)
   else:
      ps = pieces(tiles, 4)
   x0 = c * 9
   y0 = r * 9
   for p in ps:
      print(p)
      for rotation in tiles[p]:
         # print("Trying at:", c, r)
         # print(printTile(rotation))
         fit = True
         newLayout = dict(lt)
         for dx in range(10):
            for dy in range(10):
               x, y = x0+dx, y0+dy
               cc = rotation[dy*10+dx]
               if (x, y) in newLayout:
                  if cc != newLayout[(x, y)]:
                     fit = False
                     # print("Failed at", dx, dy)
                     break
               else:
                  newLayout[(x,y)] = cc
            if fit == False:
               break
         if fit == True:
            # print("Found match ", p, r, c)
            c1 = c + 1
            if c1 > 11:
               c1 = 0
               r1 = r + 1
            else:
               r1 = r
            otherTiles = dict(tiles)
            del otherTiles[p]
            # print(len(tiles), len(otherTiles))
            theLayout = layPuzzle(otherTiles, newLayout, c1, r1)
            if theLayout != None:
               return theLayout
   return None

def getSeaMonsterDelta():
   d = set()
   s = """                  # 
#    ##    ##    ###
 #  #  #  #  #  #   """
   for lidx, line in enumerate(s.splitlines()):
      for cidx, c in enumerate(line):
         if c == "#":
            d.add((cidx, lidx))
   return d

def flipSeaMonster(s):
   mxx = 0
   for x, y in s:
      mxx = max(x,mxx)
   s2 = set()
   for x, y in s:
      s2.add((mxx-x, y))
   return s2

def rotateSeaMonster(s, steps):
   if steps == 0:
      return s
   mxy = 0
   for x, y in s:
      mxy = max(y, mxy)
   s2 = set()
   for x, y in s:
      s2.add((mxy-y, x))
   return rotateSeaMonster(s2, steps-1)

@measure
def part1(pinp):
   tiles = parseTiles(pinp)
   p = 1
   for tile in pieces(tiles, 2):
      p = p * tile
   return p

@measure
def part2(pinp):
   tiles = parseTiles(pinp)
   # print(printTiles(tiles[3527]))
   layout = layPuzzle(tiles, dict())
   layout2 = dict()
   for x in range(12*10-11):
      if x % 9 == 0:
         continue
      for y in range(12*10-11):
         if y % 9 == 0:
            continue
         layout2[(x-(x//9+1), y-(y//9+1))] = layout[(x, y)]
   print(len(layout2))
   for y in range(12*8):
      for x in range(12*8):
         print(layout2[(x, y)], end="")
      print()

   seaMonsters = list()
   for rotation in range(4):
      sm = getSeaMonsterDelta()
      seaMonsters.append(rotateSeaMonster(sm, rotation))
      seaMonsters.append(flipSeaMonster(seaMonsters[-1]))
   for sm in seaMonsters:
      print(sm)
      mxFound = 0
      for x in range(12*8):
         for y in range(12*8):
            found = True
            f = 0
            for dx, dy in sm:
               if layout2.get((x+dx, y+dy), " ") != "#":
                  found = False
                  if f > mxFound:
                     print('Matched', f, x, y)
                     mxFound = f
                  break
               f += 1
            if found:
               print("Found for sm ", sm)
   return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   # inp = """"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
