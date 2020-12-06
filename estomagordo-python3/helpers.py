import re


def distance(a, b):
    return sum([(a[x]-b[x])**2 for x in range(len(a))])**0.5


def distance_sq(a, b):
    return sum([(a[x]-b[x])**2 for x in range(len(a))])


def ints(line):
    pattern = re.compile(r'-?\d+')

    return [int(val) for val in re.findall(pattern, line) if val]


def manhattan(a, b):
    return sum([abs(a[x]-b[x]) for x in range(len(a))])


def neighs(y, x):
    return [[y-1,x], [y+1,x], [y,x-1], [y,x+1]]


def neighs_bounded(y, x, rmin, rmax, cmin, cmax):
    neighs = []

    if y > rmin:
        neighs.append([y-1, x])

    if y < rmax:
        neighs.append([y+1, x])

    if x > cmin:
        neighs.append([y, x-1])

    if x < cmax:
        neighs.append([y, x+1])

    return neighs


def grouped_lines(lines):
    groups = []
    group = []

    for line in lines:
        if not line.strip():
            groups.append(group)
            group = []
        else:
            group.append(line.rstrip())

    if group:
        groups.append(group)

    return groups