
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, n_neighs, neighs, neighs_bounded


def solve(lines):
    ingredients = set()
    allergens = {}

    for i, a in lines:
        for ing in i:
            ingredients.add(ing)

    for i, a in lines:
        for all in a:
            allergens[all] = set(ingredients)

    for i, a in lines:
        for all in a:
            allergens[all] &= i

    definite = set()

    for setty in allergens.values():
        if len(setty) == 1:
            definite |= setty

    while True:
        reductdict = {}
        
        for key, setty in allergens.items():
            if len(setty - definite) == 1:
                reductdict[key] = setty - definite

        if not reductdict:
            break

        for k, v in reductdict.items():
            allergens[k] = v
            definite |= v

    out = [(k, v) for k,v in allergens.items()]
    out.sort()

    return ','.join(list(pair[1])[0] for pair in out)
    
    targets = set()

    for v in allergens.values():
        targets |= v

    count = 0

    for i, _ in lines:
        for ing in i:
            if ing not in targets:
                count += 1

    return count

if __name__ == '__main__':
    lines = []

    with open('21.txt') as f:
        for line in f.readlines():
            words = line.split()
            ingredients = set()
            allergens = []
            ingredone = False

            for word in words:
                if word[0] == '(':
                    ingredone = True
                elif ingredone:
                    allergens.append(word[:-1])
                else:
                    ingredients.add(word)

            lines.append((ingredients, allergens))

    print(solve(lines))
