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

def findSum(d, goal):
   for i in range(25):
      for j in range(i+1, 25):
         if goal == d[i] + d[j]:
            return True
   return False

@measure
def part1(pinp):
   l = map(int, pinp)
   d = deque(next(l) for i in range(25))
   idx = 25
   for goal in l:
      if not findSum(d, goal):
         return goal
      d.append(goal)
      d.popleft()
   return "Not Found"

@measure
def part2(pinp, goal):
   l = list(map(int, pinp))
   a, b = 0, 1
   sm = l[a] + l[b]
   while sm != goal:
      if sm < goal:
         b += 1
         sm += l[b]
      else:
         sm -= l[a]
         a += 1
   return max(l[a:b+1]) + min(l[a:b+1])

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    p1 = part1(parseInp)
    print("Solution to part 1: {}".format(p1))
    print("Solution to part 2: {}".format(part2(parseInp, p1)))

## End of footer boilerplate ###################################################
