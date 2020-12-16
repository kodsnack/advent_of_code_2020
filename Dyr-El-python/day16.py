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

@measure
def part1(pinp):
   lines = iter(pinp)
   fields = dict()
   field = 0
   for line in lines:
      if len(line) == 0:
         break
      for token in line:
         fields[field] = set()
         s = fields[field]
         field += 1
         if token.count("-") > 0:
            low, high = token.split("-")
            for i in range(int(low), int(high)+1):
               s.add(i)
   for line in lines:
      if len(line) == 0:
         break
   sm = 0
   for line in lines:
      if line[0] == "nearby":
         continue
      for token in line.split(","):
         valid = False
         t = int(token)
         for s in fields.values():
            if t in s:
               valid = True
         if not valid:
            sm += t
   return sm

@measure
def part2(pinp):
   lines = iter(pinp)
   fields = dict()
   fieldNames = dict()
   for field, line in enumerate(lines):
      if len(line) == 0:
         break
      for token in line:
         if field not in fields:
            fields[field] = set()
         s = fields[field]
         if token.count("-") > 0:
            low, high = token.split("-")
            for i in range(int(low), int(high)+1):
               s.add(i)
         else:
            if field not in fieldNames:
               fieldNames[field] = list()
            fieldNames[field].append(token)
   myTicket = dict()
   for line in lines:
      if len(line) == 0:
         break
      if line[0] == "your":
         continue
      for field, token in enumerate(line.split(",")):
         myTicket[field] = int(token)
   fieldLayout = list()
   for field in fields.keys():
      fieldLayout.append(set(fields.keys()))
   print(fieldLayout)
   for line in lines:
      if line[0] == "nearby":
         continue
      tokens = [int(t) for t in line.split(",")]
      validTicket = True
      for token in tokens:
         valid = False
         t = int(token)
         for s in fields.values():
            if t in s:
               valid = True
         if not valid:
            validTicket = False
      if not validTicket:
         continue
      for fieldIdx, token in enumerate(tokens):
         for possFieldIdx, field in fields.items():
            if token not in field:
               fieldLayout[possFieldIdx].remove(fieldIdx)
   change = True
   while change == True:
      change = False
      for fieldSet in fieldLayout:
         if len(fieldSet) == 1:
            for item in fieldSet:
               for fs in fieldLayout:
                  if fs != fieldSet and item in fs:
                     fs.remove(item)
                     change = True
      print(fieldLayout)
   p = 1
   for fieldNo, fieldSet in enumerate(fieldLayout):
      if fieldNames[fieldNo][0] == "departure":
         print(fieldSet)
         for item in fieldSet:
            p *= myTicket[item]
   return p

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
   # inp = """"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
