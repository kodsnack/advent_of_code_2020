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

def joltages(pinp):
   result = sorted(list(map(int, pinp)))
   return [0] + result + [max(result)+3]

def part1(pinp):
   j = joltages(pinp)
   print(j)
   diffs = [y-x for x, y in zip(j, j[1:])]
   return diffs.count(1)*diffs.count(3)

def part2(pinp):
   j = joltages(pinp)
   d = {0:1}
   for i in j[1:]:
      d[i] = d.get(i-1, 0) + d.get(i-2, 0) + d.get(i-3, 0)
   return d[max(j)]   

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
