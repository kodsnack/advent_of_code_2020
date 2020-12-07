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

from collections import deque

def countBags(bag, bags):
   if bag in bags:
      sm = 0
      for bt, bn in bags[bag]:
         print(bn, countBags(bt, bags))
         sm += (bn * countBags(bt, bags))
      print(sm)
      return sm + 1
   return 1

def part1(pinp):
   d = {}
   rd = {}
   for bag in pinp:
      container = bag[0]+" "+bag[1]
      if container not in bag:
         d[container] = []
      if bag[4] != "no":
         idx = 4
         while len(bag) > idx + 2:
            noBags = int(bag[idx])
            typeBags = bag[idx+1]+" "+bag[idx+2]
            if typeBags not in rd:
               rd[typeBags] = []
            rd[typeBags].append(container)
            d[container].append([typeBags, noBags])
            idx += 4
   b = deque()
   s = set()
   b.append("shiny gold")
   while len(b)>0:
      bag = b.popleft()
      if bag not in rd:
         s.add(bag)
         continue
      for cb in rd[bag]:
         if cb not in s:
            b.append(cb)
            s.add(cb)
   if "shiny gold" in b:
      print("self contained")
      b.remove("shiny gold")
   print(len(s))
   print(countBags("shiny gold", d))
   return "<solution1>"

def part2(pinp):
    return "<solution2>"

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
