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

def parsePlayer(pinp):
   l = list()
   playerNo = 0
   for line in pinp:
      if line == "":
         yield playerNo, l
         l = list()
         continue
      if line.startswith("Player"):
         playerNo = int(line[-2])
      else:
         l.append(int(line))
   if len(l)>0:
      yield playerNo, l

def pround(p1, p2):
   if p1[0] > p2[0]:
      p1.append(p1[0])
      p1.append(p2[0])
      del p1[0]
      del p2[0]
   else:
      p2.append(p2[0])
      p2.append(p1[0])
      del p1[0]
      del p2[0]

def crabs(p1, p2):
   while True:
      c1, c2 = p1[0], p2[0]
      del p1[0]
      del p2[0]
      if c1 > c2:
         p1.extend([c1, c2])
      else:
         p2.extend([c2, c1])
      if len(p1)==0:
         return 2
      elif len(p2)==0:
         return 1

def calcKey(p1, p2):
   d = {c:i for i, c in enumerate(sorted(p1+p2))}
   return (tuple(d[x] for x in p1), tuple(d[x] for x in p2))

gameCache = dict()
def recCrabs(p1, p2, lvl=0):
   gameKey = calcKey(p1, p2)
   if gameKey in gameCache:
      return gameCache[gameKey]
   cache = set()
   while True:
      key = (tuple(p1), tuple(p2))
      if key in cache:
         return 1
      cache.add(key)
      c1, c2 = p1[0], p2[0]
      del p1[0]
      del p2[0]
      if c1<= len(p1) and c2<=len(p2):
         winner = recCrabs(p1[:c1], p2[:c2])
      elif c1>c2:
         winner = 1
      else:
         winner = 2
      if winner == 1:
         p1.append(c1)
         p1.append(c2)
      else:
         p2.append(c2)
         p2.append(c1)
      if len(p1)==0:
         gameCache[gameKey] = 2
         return 2
      if len(p2)==0:
         gameCache[gameKey] = 1
         return 1

@measure
def part1(pinp):
   i = parsePlayer(pinp)
   p1 = next(i)[1]
   p2 = next(i)[1]
   if crabs(p1, p2)==1:
      return sum(map(lambda x, y: x*y, p1, range(len(p1), 0, -1)))
   else:
      return sum(map(lambda x, y: x*y, p2, range(len(p2), 0, -1)))

@measure
def part2(pinp):
   i = parsePlayer(pinp)
   p1 = next(i)[1]
   p2 = next(i)[1]
   if recCrabs(p1, p2)==1:
      return sum(map(lambda x, y: x*y, p1, range(len(p1), 0, -1)))
   else:
      return sum(map(lambda x, y: x*y, p2, range(len(p2), 0, -1)))

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
