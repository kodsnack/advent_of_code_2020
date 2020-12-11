import sys
from globalFunctions import readInputFile


def solvePart1():
    inp = readInputFile('../data/Day01Input')
    inp_size = len(inp)
    for i in range(inp_size - 1):
        inp[i] = int(inp[i])
        for j in range(i + 1, inp_size):
            inp[j] = int(inp[j])
            if inp[i] + inp[j] == 2020:
                return inp[i] * inp[j]


def solvePart2():
    inp = readInputFile('../data/Day01Input')
    inp_size = len(inp)
    for i in range(inp_size - 2):
        inp[i] = int(inp[i])
        for j in range(i + 1, inp_size - 1):
            inp[j] = int(inp[j])
            for z in range(j + 1, inp_size):
                inp[z] = int(inp[z])
                if inp[i] + inp[j] + inp[z] == 2020:
                    return inp[i] * inp[j] * inp[z]


print(solvePart1())
print(solvePart2())
