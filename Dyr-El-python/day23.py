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

class LL:
   d = dict()
   def __init__(self, v):
      self.next = None
      self.prev = None
      self.v = v
      LL.d[v] = self
   def remove(self):
      nxt, prv = self.next, self.prev
      nxt.prev, prv.next = prv, nxt
   def add(self, after):
      self.next = after.next
      self.prev = after
      after.next = self
      self.next.prev = self
   @staticmethod
   def createCircle(l, maxV=None):
      root = LL(l[0])
      current = root
      for i in l[1:]:
         node = LL(i)
         node.prev = current
         current.next = node
         current = node
      if maxV == None:
         maxV = max(l)
      for i in range(max(l)+1, maxV+1):
         node = LL(i)
         node.prev = current
         current.next = node
         current = node
      current.next = root
      root.prev = current
      return root
   def remove(self, no):
      before = self.prev
      current = self
      l = [current.v]
      for i in range(no-1):
         current = current.next
         l.append(current.v)
      before.next = current.next
      current.next.prev = before
      self.prev = current
      current.next = self
      return l
   def addSeq(self, sq):
      first, last = sq, sq.prev
      first.prev = self
      last.next = self.next
      self.next.prev = last
      self.next = first
   def find(self, v):
      return LL.d[v]
   def print(self, steps=10):
      current = self
      l = list()
      for i in range(steps):
         l.append(str(current.v))
         current = current.next
         if current==self:
            break
      return ",".join(l)

def doMove(l, steps=1, mxV=None):
   root = LL.createCircle(l, mxV)
   current = root
   for i in range(steps):
      pickOut = current.next
      lv = pickOut.remove(3)
      v = current.v - 1
      if v == 0:
         v = mxV
      while v in lv:
         v -= 1
         if v == 0:
            v = mxV
      dest = current.find(v)
      dest.addSeq(pickOut)
      current = current.next
   return root

@measure
def part1(pinp):
   inp = list(map(int, pinp[0]))
   n = doMove(inp, 100, 9)
   c = n.find(1).next
   result = []
   while c.v != 1:
      result.append(str(c.v))
      c = c.next
   return int(''.join(str(i) for i in result))

@measure
def part2(pinp):
   inp = list(map(int, pinp[0]))
   n = doMove(inp, 10000000, 1000000)
   c1 = n.find(1).next
   c2 = c1.next
   return c1.v*c2.v

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   # inp = """389125467"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
