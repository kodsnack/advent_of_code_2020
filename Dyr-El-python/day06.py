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
    return tuple(map(lambda x:x[0] if len(x)>0 else x, 
                    map(lambda x:lineparser(x, tokenparser, tokenPattern),
                        inp.splitlines())))

## End of header boilerplate ###################################################

from functools import reduce

def questionSetReduce(pinp, f, init):
   groups = list()
   groups.append(set(init))
   for person in pinp:
      if person == ():
         groups.append(set(init))
         continue
      groups[-1] = f(groups[-1], set(person))
   return sum(len(group) for group in groups)

def part1(pinp):
   return questionSetReduce(pinp, lambda s, x:s|x, "")

def part2(pinp):
   return questionSetReduce(pinp, lambda s, x:s&x, "abcdefghijklmnopqrstuvwxyz")

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
#     inp = """abc

# a
# b
# c

# ab
# ac

# a
# a
# a
# a

# b"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
