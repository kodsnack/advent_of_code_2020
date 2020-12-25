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

from math import gcd

@measure
def part1(pinp):
   cardPK, doorPK = int(pinp[0]), int(pinp[1])
   v, doorV, cardV = 1, 1, 1
   while True:
      v = (v * 7) % 20201227
      doorV = (doorV * cardPK) % 20201227
      cardV = (cardV * doorPK) % 20201227
      if v == cardPK:
         return cardV
      if v == doorPK:
         return doorV

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")

## End of footer boilerplate ###################################################
