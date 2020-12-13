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

def part1(pinp):
   time = int(pinp[0])
   bussIds = [int(i) for i in pinp[1].split(",") if i!="x"]
   departureTimes = list()
   for buss in bussIds:
      if time % buss == 0:
         return 0
      else:
         departureTimes.append((buss - time%buss, buss))
   departureTimes.sort()
   return departureTimes[0][0]*departureTimes[0][1]

from math import gcd

def part2(pinp):
   bussIds = [(offset, int(ids))
              for offset, ids in enumerate(pinp[1].split(",")) 
              if ids != "x"]
   time, currentStep, usedSteps = 0, 1, set()
   found = False
   while not found:
      time += currentStep
      found = True
      for offset, bussId in bussIds:
         if (time+offset) % bussId != 0:
            found = False
            break
         else:
            if bussId not in usedSteps:
               usedSteps.add(bussId)
               currentStep = bussId * currentStep // gcd(bussId, currentStep)
   return time

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   inp2 = """939
7,13,x,x,59,x,31,19"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print("Solution to part 1: {}".format(part1(parseInp)))
   print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
