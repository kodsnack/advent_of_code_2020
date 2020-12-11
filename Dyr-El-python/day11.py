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

def part1(pinp):
   d = dict()
   for lidx, line in enumerate(pinp):
      for colidx, seat in enumerate(line):
         if seat != '.':
            d[(colidx, lidx)] = seat
   while True:
      change = False
      d2 = dict()
      for colidx, lidx in d:
         seat = d[(colidx, lidx)]
         adj = []
         for dc, dl in ((-1, -1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)):
            nc, nl = colidx+dc, lidx+dl
            adj.append(d.get((nc, nl), ' '))
         if seat == "#" and adj.count("#")>=4:
            d2[(colidx, lidx)] = "L"
            change = True
         elif seat == "L" and adj.count("#")==0:
            d2[(colidx, lidx)] = "#"
            change = True
         else:
            d2[(colidx, lidx)] = d[(colidx, lidx)]
      if not change:
         return ''.join(d.values()).count("#")
      else:
         d = d2

   return "<solution1>"

def part2(pinp):
   d = dict()
   for lidx, line in enumerate(pinp):
      for colidx, seat in enumerate(line):
         if seat != '.':
            d[(colidx, lidx)] = seat
   while True:
      change = False
      d2 = dict()
      for colidx, lidx in d:
         seat = d[(colidx, lidx)]
         adj = []
         for dc, dl in ((-1, -1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)):
            for i in range(1, 100):
               nc, nl = colidx+dc*i, lidx+dl*i
               if d.get((nc, nl),' ') in "L#":
                  adj.append(d.get((nc, nl), ' '))
                  break
               if nc<0 or nl<0 or nc>100 or nl>100:
                  break
         if (colidx, lidx) == (0, 0):
            print(adj)
         if seat == "#" and adj.count("#")>=5:
            d2[(colidx, lidx)] = "L"
            change = True
         elif seat == "L" and adj.count("#")==0:
            d2[(colidx, lidx)] = "#"
            change = True
         else:
            d2[(colidx, lidx)] = d[(colidx, lidx)]
      if not change:
         return ''.join(d.values()).count("#")
      else:
         print(''.join(d.values()).count("#"))
         # for line in range(10):
         #    print(''.join(d2.get((col, line), '.') for col in range(10)))
         d = d2

   return "<solution2>"

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
   #  inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
