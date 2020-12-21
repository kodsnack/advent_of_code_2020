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

def foodParser(pinp):
   i = set()
   a = set()
   for food in pinp:
      fi = iter(food)
      for ingredient in fi:
         if ingredient == "(contains":
            break
         else:
            i.add(ingredient)
      for allergen in fi:
         if allergen[-1] in "),":
            a.add(allergen[:-1])
         else:
            a.add(allergen)
      yield (i,a)
      i = set()
      a = set()

@measure
def part1(pinp):
   foods = list()
   pAllergicI = dict()
   allI = set()
   for i, a in foodParser(pinp):
      foods.append((i, a))
      allI |= i
      for allergen in a:
         if allergen not in pAllergicI:
            pAllergicI[allergen] = i
         else:
            pAllergicI[allergen] = pAllergicI[allergen] & i
   canBeInOne = set()
   for key in pAllergicI:
      canBeInOne |= pAllergicI[key]
      print(key, len(pAllergicI[key]))
   print(canBeInOne)
   canBeInNone = allI - canBeInOne
   print(canBeInNone)
   # change = True
   # while change:
   #    change = False
   #    for key in pAllergicI:
   #       if len(pAllergicI[key])==1:
   #          for allergen in pAllergicI[key]:
   #             for k2 in pAllergicI:
   #                if k2 == key:
   #                   continue
   #                if allergen in pAllergicI[k2]:
   #                   pAllergicI[k2].remove(allergen)
   #                   change = True
   # safe = set()
   # for key, v in pAllergicI.items():
   #    safe |= v
   # print(pAllergicI)
   # print(safe)
   sm = 0
   for i, a in foods:
      for ingredient in i:
         if ingredient in canBeInNone:
            sm += 1
   return sm

@measure
def part2(pinp):
   foods = list()
   pAllergicI = dict()
   allI = set()
   for i, a in foodParser(pinp):
      foods.append((i, a))
      allI |= i
      for allergen in a:
         if allergen not in pAllergicI:
            pAllergicI[allergen] = i
         else:
            pAllergicI[allergen] = pAllergicI[allergen] & i
   canBeInOne = set()
   for key in pAllergicI:
      canBeInOne |= pAllergicI[key]
   change = True
   while change:
      change = False
      for key in pAllergicI:
         if len(pAllergicI[key])==1:
            for allergen in pAllergicI[key]:
               for k2 in pAllergicI:
                  if k2 == key:
                     continue
                  if allergen in pAllergicI[k2]:
                     pAllergicI[k2].remove(allergen)
                     change = True
   l = list()
   for k, s in pAllergicI.items():
      l.append((k, list(s)[0]))
   return ",".join(x[1] for x in sorted(l))

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
#    inp = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
# trh fvjkl sbzzf mxmxvkd (contains dairy)
# sqjhc fvjkl (contains soy)
# sqjhc mxmxvkd sbzzf (contains fish)"""
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
