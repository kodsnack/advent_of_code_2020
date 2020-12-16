
from collections import Counter, defaultdict, deque
from functools import reduce
from heapq import heappop, heappush
from itertools import combinations, permutations, product

from helpers import distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, neighs, neighs_bounded


def solve(fields, your, other):
    valid_tickets = []

    fieldnames = {}

    for field in fields:
        s = set()

        for v in range(field[1], field[2]+1):
            s.add(v)
        for v in range(field[3], field[4]+1):
            s.add(v)

        fieldnames[field[0]] = [s]

    for ticket in other:
        allvalid = True

        for val in ticket:
            valid = False

            for field in fieldnames.values():
                if val in field[0]:
                    valid = True

            if not valid:
                allvalid = False
        if allvalid:
            valid_tickets.append(ticket)

    for k in fieldnames.keys():
        fieldnames[k].append({x for x in range(len(valid_tickets[0]))})

    for fieldno in range(len(valid_tickets[0])):
        for k in fieldnames.keys():
            valid = True

            for valti in valid_tickets:
                if valti[fieldno] not in fieldnames[k][0]:
                    valid = False
                    break

            if not valid:
                fieldnames[k][1].remove(fieldno)

    decided = set()

    while any(len(v[1]) > 1 for v in fieldnames.values()):
        for k, v in fieldnames.items():
            if len(v[1] - decided) == 1:
                fieldnames[k][1] -= decided
                decided |= fieldnames[k][1]

    departures = [position for k, v in fieldnames.items() for position in v[1] if 'departure' in k]

    val = 1

    for dep in departures:
        val *= your[dep]

    return val

if __name__ == '__main__':
    fields = []
    fieldsfound = False
    your = []
    other = []

    with open('16.txt') as f:
        for line in f.readlines():
            if not line.strip():
                fieldsfound = True
            elif not fieldsfound:
                fieldslist = line.split()
                name = ''

                for field in fieldslist:
                    if field[-1] == ':':
                        name += ' ' + field[:-1]
                        break
                    name += ' ' + field

                nums = [abs(n) for n in ints(line)]

                fields.append([name.strip()] + nums)

            elif not your:
                your = ints(line)
            elif ints(line):
                other.append(ints(line))

    print(solve(fields, your, other))