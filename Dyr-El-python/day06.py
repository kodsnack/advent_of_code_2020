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
                   tokenPattern=wsTokenPattern):
    return tuple(map(lambda x:x, 
                    map(lambda x:lineparser(x, tokenparser, tokenPattern),
                        inp.split("\n\n"))))

## End of header boilerplate ###################################################

from functools import reduce

def questionSetReduce(pinp, f, init):
   groups = (reduce(f, map(set, group), set(init)) for group in pinp)
   return sum(len(group) for group in groups)

def part1(pinp):
   return questionSetReduce(pinp, lambda s, x:s|x, "")

def part2(pinp):
   return questionSetReduce(pinp, lambda s, x:s&x, "abcdefghijklmnopqrstuvwxyz")

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
