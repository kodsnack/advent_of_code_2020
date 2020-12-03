## Start of header boilerplate #################################################

from aocbase import readInput
import re

oneLinePattern = re.compile(r"^.*$")
wsTokenPattern = re.compile(r"\S+")

def lineParse(s, f, fp):
    m = re.findall(fp, s)
    if m==None:
        raise ValueError("No token patterns found")
    l, h = map(int, m[0].split('-'))
    c = m[1][:-1]
    return (l, h, c, m[2])

def fileParse(inp, lineparser=lineParse,
                   tokenparser=lambda x:x,
                   tokenPattern=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:x if len(x)>1 else x[0], 
                    map(lambda x:lineparser(x, tokenparser, tokenPattern),
                        inp.splitlines())))

## End of header boilerplate ###################################################

def part1(pinp):
    return sum(l<=s.count(c)<=h for l,h,c,s in pinp)

def part2(pinp):
    return sum((s[i1 - 1] == c) != (s[i2 - 1] == c) for i1, i2, c, s in pinp)

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
