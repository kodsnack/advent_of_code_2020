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

@measure
def part1(pinp):
   start = list(map(int, pinp[0].split(",")))
   idx = 0
   d = dict()
   while idx != 2020:
      idx += 1
      if idx <= len(start):
         number = start[idx-1]
      else:
         if lastNumber in d:
            number = idx - d[lastNumber] - 1
         else:
            number = 0
      print(idx, number)
      if idx > 1:
         d[lastNumber] = idx - 1
      lastNumber = number
   return number

@measure
def part2(pinp):
   start = list(map(int, pinp[0].split(",")))
   idx = 0
   d = dict()
   while idx != 30000000:
      idx += 1
      if idx <= len(start):
         number = start[idx-1]
      else:
         if lastNumber in d:
            number = idx - d[lastNumber] - 1
         else:
            number = 0
      if idx > 1:
         d[lastNumber] = idx - 1
      lastNumber = number
   return number

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   # inp = """3,1,2"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
