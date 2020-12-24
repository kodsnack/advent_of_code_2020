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

from functools import reduce

def possiblyAllergenicIngredients(pinp):
   pAllergicI = dict()
   for ingredients, allergens in foodParser(pinp):
      for allergen in allergens:
         if allergen not in pAllergicI:
            pAllergicI[allergen] = ingredients
         else:
            pAllergicI[allergen] = pAllergicI[allergen] & ingredients
   return pAllergicI
   
@measure
def part1(pinp):
   foods = [(i, a) for i, a in foodParser(pinp)]
   allIngredients = reduce(lambda x, y: x|y, (i for (i, a) in foodParser(pinp)))
   possAllergiecIngredients = possiblyAllergenicIngredients(pinp)
   canBeInOne = reduce(lambda x,y: x|y, possAllergiecIngredients.values())
   canBeInNone = allIngredients - canBeInOne
   return len([i for ingredients, allergens in foods for i in ingredients
              if i in canBeInNone])

@measure
def part2(pinp):
   foods = [(i, a) for i, a in foodParser(pinp)]
   possAllergiecIngredients =  possiblyAllergenicIngredients(pinp)
   canBeInOne = set()
   canBeInOne = reduce(lambda x,y: x|y, possAllergiecIngredients.values())
   change = True
   while change:
      change = False
      for key in possAllergiecIngredients:
         if len(possAllergiecIngredients[key])==1:
            for allergen in possAllergiecIngredients[key]:
               for k2 in possAllergiecIngredients:
                  if k2 == key:
                     continue
                  if allergen in possAllergiecIngredients[k2]:
                     possAllergiecIngredients[k2].remove(allergen)
                     change = True
   l = [(k, list(s)[0]) for k, s in possAllergiecIngredients.items()]
   return ",".join(x[1] for x in sorted(l))

## Start of footer boilerplate #################################################

if __name__ == "__main__":
   inp = readInput()
    
   ## Update for input specifics ##############################################
   parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

   print("Input is '" + str(parseInp[:10])[:160] + 
         ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
   print(f"Solution to part 1: {part1(parseInp)}")
   print(f"Solution to part 2: {part2(parseInp)}")

## End of footer boilerplate ###################################################
