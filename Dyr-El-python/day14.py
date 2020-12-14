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

def parser(pinp):
   for s in pinp:
      if s[0] == "mask":
         mask = s[2]
      else:
         address = int(s[0][4:-1])
         value = int(s[2])
         yield (mask, address, value)

@measure
def part1(pinp):
   mem = dict()
   for mask, address, value in parser(pinp):
      bitMask = int(mask.replace("1", "0").replace("X", "1"), 2)
      mem[address] = (value & bitMask) | int(mask.replace("X", "0"), 2)
   return sum(mem.values())

@measure
def part2(pinp):
   mem = dict()
   for mask, address, value in parser(pinp):
      for bitPattern in range(2**mask.count("X")):
         bitList = list()
         addressBitPattern = address
         for c in reversed(mask):
            if c == '0':
               bitList.append(str(addressBitPattern & 1))
            elif c == '1':
               bitList.append(c)
            else:
               bitList.append(str(bitPattern & 1))
               bitPattern >>= 1
            addressBitPattern >>= 1
         newAddress = int(''.join(reversed(bitList)),2)
         mem[newAddress] = value
   return sum(mem.values())

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   inp2 = """mask = XXXX
mem[0] = 1
mask = XX11
mem[1] = 0
"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
