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

def findChain(d, j, s, t, tot):
   # print(j, t, s)
   if j == t-3:
      if len(s) == tot:
         return []
      else:
         print(len(s))
         return None
   if j in d:
      l = d[j]
      for i in sorted(l):
         if i in s:
            continue
         s2=set(s)
         s2.add(i)
         r = findChain(d, i, s2, t, tot)
         if r != None:
            return [i] + r
   return None


def part1(pinp):
   joltages = list(map(int, pinp))
   d = dict()
   target = max(joltages)+3
   for j in joltages:
      for v in range(1, 4):
         if j-v not in d:
            d[j-v] = list()
         d[j-v].append(j)
   chain = findChain(d, 0, set(), target, len(joltages))
   chain = [0] + chain + [target]
   s1, s3 = 0, 0
   for i, j in zip(chain, chain[1:]):
      if j-i==1:
         s1+=1
      if j-i==3:
         s3+=1
   print(chain)
   print(s1, s3, s1*s3)
   return "<solution1>"

cache = {}
def countWays(d, s, target):
   print(target, s)
   fs = frozenset(s)
   if (fs, target) in cache:
      return cache[(fs, target)]
   sm = 0
   for i in range(1, 4):
      nxt = target - i
      if nxt in d and nxt not in s:
         s2 = set(s)
         s2.add(nxt)
         sm += countWays(d, s2, nxt)
   cache[(fs, target)] = sm
   return sm

def noWays(l, idx):
   if idx > len(l) - 2:
      return 1
   if l[idx+1] - l[idx-1] > 3:
      v = noWays(l, idx+1)
      return v
   l2 = l[:idx] + l[idx+1:]
   v = noWays(l, idx+1) + noWays(l2, idx)
   return v
   

def part2(pinp):
   joltages = [0] + list(map(int, pinp))
   joltages.append(max(joltages)+3)
   joltages.sort()
   print(joltages)
   print(noWays(joltages, 1))
   # d = dict()
   # for j in joltages:
   #    for v in range(1, 4):
   #       if j+v not in d:
   #          d[j+v] = list()
   #       d[j+v].append(j)
   # nw = {target:1}
   # print(countWays(d, set(), target))
   

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """28
# 33
# 18
# 42
# 31
# 14
# 46
# 20
# 48
# 47
# 24
# 23
# 49
# 45
# 19
# 38
# 39
# 11
# 1
# 32
# 25
# 35
# 8
# 17
# 7
# 9
# 4
# 2
# 34
# 10
# 3"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
