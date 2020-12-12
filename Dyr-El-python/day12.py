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

import cmath
from math import pi

def directions(pinp):
   for s in pinp:
      yield s[0], int(s[1:])

def move(pos, direction, distance):
   return pos + direction*distance

def rotate(vector, angle):
   return vector * cmath.rect(1, angle)

def part1(pinp):
   pos, heading = 0, 1
   movements = {"N":0+1j, "S":0-1j, "E":1, "W":-1}
   headings = {"L":pi/180, "R":-pi/180}
   for cmd, arg in directions(pinp):
      if cmd in movements:
         pos = move(pos, movements[cmd], arg)
      elif cmd in headings:
         heading = rotate(heading, arg*headings[cmd])
      elif cmd == "F":
         pos = move(pos, heading, arg)
   return round(abs(pos.real)+abs(pos.imag))

def part2(pinp):
   wp, pos = 10+1j, 0
   movements = {"N":0+1j, "S":0-1j, "E":1, "W":-1}
   headings = {"L":pi/180, "R":-pi/180}
   for cmd, arg in directions(pinp):
      if cmd in movements:
         wp = move(wp, movements[cmd], arg)
      elif cmd in headings:
         wp = rotate(wp, arg*headings[cmd])
      elif cmd == "F":
         pos = move(pos, wp, arg)
   return round(abs(pos.real)+abs(pos.imag))

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    inp2 = """F10
N3
F7
R90
F11"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
