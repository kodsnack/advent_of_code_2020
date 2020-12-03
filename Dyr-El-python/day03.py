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
    return tuple(map(lambda x:x if len(x)>1 else x[0], 
                    map(lambda x:lineparser(x, tokenparser, tokenPattern),
                        inp.splitlines())))

## End of header boilerplate ###################################################

def parseWood(lines):
   d = dict()
   for lidx, line in enumerate(lines):
      for colidx, c in enumerate(line):
         if c == "#":
            d[(colidx, lidx)] = True
   d["lines"] = len(lines)
   d["columns"] = len(lines[0])
   return d

def traverse(d, deltax, deltay):
   x, y = (0, 0)
   height, width = d["lines"], d["columns"]
   noTrees = 0
   while y < height:
      noTrees += ((x, y) in d)
      x, y = (x + deltax) % width, y + deltay
   return noTrees

def part1(pinp):
   d = parseWood(pinp)
   return traverse(d, 3, 1)

def part2(pinp):
   d = parseWood(pinp)
   p = 1
   for deltax, deltay in ((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)):
      p *= traverse(d, deltax, deltay)
   return p

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
