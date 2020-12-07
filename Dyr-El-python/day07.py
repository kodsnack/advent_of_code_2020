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
from itertools import count

def countBags(theBag, bagMap):
   return sum(noBags*countBags(nextBag, bagMap) 
              for nextBag, noBags in bagMap.get(theBag, [])) + 1

def parseBags(l):
   for bagDef in l:
      container, content = (bagDef[0] + " " + bagDef[1]), []
      for idx in count(start=4, step=4):
         try:
            noBags, bagType = int(bagDef[idx]), bagDef[idx+1] + " " + bagDef[idx+2]
            content.append((bagType, noBags))
         except:
            yield (container, content)
            break

from collections import defaultdict

def buildReverseBagTree(bags):
   reverseBag = defaultdict(list)
   for bagType, content in bags:
      for containedBagType, noBags in content:
         reverseBag[containedBagType].append(bagType)
   return reverseBag

def part1(pinp):
   revBag = buildReverseBagTree(parseBags(pinp))
   bagQueue = deque(["shiny gold"])
   sgContainers = set()
   while len(bagQueue) > 0:
      bag = bagQueue.popleft()
      for containingBag in revBag[bag]:
         if containingBag not in sgContainers:
            sgContainers.add(containingBag)
            bagQueue.append(containingBag)
   return len(sgContainers)

def buildBagTree(bags):
   return {bagType:content for bagType, content in bags}

def part2(pinp):
   return countBags("shiny gold", buildBagTree(parseBags(pinp))) - 1

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
