import re
from itertools import product

d = dict()
with open('input.txt') as f:
    for l in f:
        l = l.strip()
        result = re.match(r'^(mask) = ([X01]{36})$', l)
        if not result:
            result = re.match(r'^(mem)\[(\d+)\] = (\d+)$', l)
        if result.group(1) == "mask":
            mask = result.group(2)
            inverse_mask = int(mask.replace('1','0').replace('X','1'),2)
            start_value = int(mask.replace('X','0'), 2)
        else:
            index = int(result.group(2))
            value = int(result.group(3))
            d[index] = start_value | (value & inverse_mask)
s = sum([v for v in d.values()])
print("Part 1", s)