## Start of header boilerplate #################################################

from aocbase import readInput
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

class Seating:
   def parseInput(self, inpStr):
      d = dict()
      for lineIdx, line in enumerate(inpStr):
         for columnIdx, c in enumerate(line):
            if c in "L#":
               d[(lineIdx, columnIdx)] = c
      return d
   def __init__(self, inpStr):
      self._d = self.parseInput(inpStr)
      self._minLine = min(line for line, col in self._d)
      self._maxLine = max(line for line, col in self._d)
      self._minColumn = min(col for line, col in self._d)
      self._maxColumn = max(col for line, col in self._d)
      self._neighbourCache = dict()
   def newSeating(self):
      return self
   def __enter__(self):
      self._dmask = dict()
      return self
   def __exit__(self, etype, eval, etraceback):
      self._lastResating = len(self._dmask)
      self._d.update(self._dmask)
      return False
   def seats(self):
      return self._d.keys()
   def occupied(self, seat):
      return self[seat] == "#"
   def free(self, seat):
      return self[seat] == "L"
   def neighbours(self, seat, distance):
      if (seat, distance) in self._neighbourCache:
         return self._neighbourCache[(seat, distance)]
      l = list()
      for deltaLine in range(-1, 2):
         for deltaColumn in range(-1, 2):
            if deltaLine == 0 and deltaColumn == 0:
               continue
            for i in range(1, distance+1):
               neighbourSeat = (seat[0]+deltaLine*i, seat[1]+deltaColumn*i)
               if neighbourSeat in self._d:
                  l.append(neighbourSeat)
                  break
               if neighbourSeat[0] < self._minLine or neighbourSeat[0] > self._maxLine:
                  break
               if neighbourSeat[1] < self._minColumn or neighbourSeat[1] > self._maxColumn:
                  break
      self._neighbourCache[(seat, distance)] = l
      return l
   def crowded(self, seat, distance, limit):
      noNeighbors = sum(self.occupied(neighbour) for neighbour in self.neighbours(seat, distance))
      return noNeighbors >= limit
   def __getitem__(self, key):
      return self._d.get(key, ".")
   def __setitem__(self, key, value):
      self._dmask[key] = value
   def occupy(self, seat):
      self[seat] = "#"
   def leave(self, seat):
      self[seat] = "L"
   @property
   def lastReseatingChanged(self):
      return self._lastResating > 0
   def totalOccupied(self):
      return sum(self.occupied(seat) for seat in self.seats())
   def step(self, dist=1, crowded=4):
      with self.newSeating():
         for seat in self.seats():
            if self.occupied(seat) and self.crowded(seat, dist, crowded):
               self.leave(seat)
            elif self.free(seat) and not self.crowded(seat, dist, 1):
               self.occupy(seat)
      return self.lastReseatingChanged
   def __str__(self):
      lines = list()
      for line in range(self._minLine, self._maxLine+1):
         columns = []
         for column in range(self._minColumn, self._maxColumn+1):
            columns.append(self._d.get((line, column), '.'))
         lines.append(''.join(columns))
      return '\n'.join(lines)

def part1(pinp):
   seating = Seating(pinp)
   steps = 0
   while seating.step():
      steps += 1
      # print(seating)
      # print(steps, seating.totalOccupied())
   return seating.totalOccupied()
   
def part2(pinp):
   seating = Seating(pinp)
   steps = 0
   while seating.step(100, 5):
      steps += 1
      # print(seating)
      # print(steps, seating.totalOccupied())
   return seating.totalOccupied()

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    inp2 = """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
