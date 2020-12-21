
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def solve(lines):
    ingredients = reduce(lambda a,b: a|b, [line[0] for line in lines])
    allergens = {allergen: set(ingredients) for allergen in reduce(lambda a,b: a|b, [line[1] for line in lines])}

    for ing, all in lines:
        for allergen in all:
            allergens[allergen] &= ing

    definite = set()

    for ingredientset in allergens.values():
        if len(ingredientset) == 1:
            definite |= ingredientset

    while True:
        reductdict = {}
        
        for key, ingredientset in allergens.items():
            if len(ingredientset - definite) == 1:
                reductdict[key] = ingredientset - definite

        if not reductdict:
            break

        for k, v in reductdict.items():
            allergens[k] = v
            definite |= v

    return ','.join(list(pair[1])[0] for pair in sorted((k, v) for k,v in allergens.items()))

if __name__ == '__main__':
    lines = []

    with open('21.txt') as f:
        for line in f.readlines():
            words = line.split()
            ingredients = set()
            allergens = set()
            ingredone = False

            for word in words:
                if word[0] == '(':
                    ingredone = True
                elif ingredone:
                    allergens.add(word[:-1])
                else:
                    ingredients.add(word)

            lines.append((ingredients, allergens))

    print(solve(lines))
