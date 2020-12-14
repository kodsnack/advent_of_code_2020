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
            bit_count = mask.count('X')
            x_combinations = list(product(range(2), repeat=bit_count))
            mask_combinations = []
            for x in x_combinations:
                i = 0
                new_mask = 0
                for c in mask:
                    if c == "X":
                        new_mask = (new_mask << 1) + x[i]
                        i += 1
                    else:
                        new_mask = (new_mask << 1) + int(c,2)
                mask_combinations.append(new_mask)
            inverse_mask = int(mask.replace('0','1').replace('X','0'),2)
        elif result.group(1) == "mem":
            index = int(result.group(2))
            value = int(result.group(3))
            for m in mask_combinations:
                i = (index & inverse_mask) | m
                d[i] = value
s = sum([v for v in d.values()])
print("Part 2:", s)