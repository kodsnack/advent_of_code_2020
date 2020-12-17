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

def fieldTypes(pinp):
   for line in pinp:
      if len(line) == 0:
         break
      fieldName, rest =  line.split(":")
      s = set()
      for token in rest.split(" "):
         if token.count("-") == 1:
            low, high = map(int, token.split("-"))
            s.update(i for i in range(low, high + 1))
      yield fieldName, s

def myTicket(pinp):
   nextLine = False
   for line in pinp:
      if nextLine:
         return {fieldIdx:fieldContent 
                 for fieldIdx, fieldContent in enumerate(map(int, line.split(",")))}
      if line == "your ticket:":
         nextLine = True

def otherTickets(pinp):
   inp = iter(pinp)
   for line in inp:
      if line == "nearby tickets:":
         break
   for line in inp:
      yield {fieldIdx:fieldContent 
             for fieldIdx, fieldContent in enumerate(map(int, line.split(",")))}

def invalidFields(ticket, validNumbers):
   return set(ticketField
              for ticketField in ticket.values()
              if ticketField not in validNumbers)

def validTickets(ticketIterator, validNumbers):
   for ticket in ticketIterator:
      if len(invalidFields(ticket, validNumbers)) == 0:
         yield ticket

@measure
def part1(pinp):
   s = set()
   for fieldName, fieldSet in fieldTypes(pinp):
      s.update(fieldSet)
   totalSum = 0
   for ticket in otherTickets(pinp):
      totalSum += sum(invalidFields(ticket, s))
   return totalSum

@measure
def part2(pinp):
   s = set()
   theFieldTypes = dict()
   for fieldName, fieldSet in fieldTypes(pinp):
      s.update(fieldSet)
      theFieldTypes[fieldName] = fieldSet
   positionValues = dict()
   for ticket in validTickets(otherTickets(pinp), s):
      for key, value in ticket.items():
         if key not in positionValues:
            positionValues[key] = set()
         positionValues[key].add(value)
   for fieldName, fieldSet in theFieldTypes.items():
      for fieldNumber, fieldSet in positionValues.items():
         if fie
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
   parseInp = fileParse(inp, tokenPattern=oneLinePattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
