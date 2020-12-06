def get_input():
    with open("input_day6.txt", 'r') as input_file:
        numbers = input_file.readlines()
    return numbers


def solve_a():
    lines = get_input()

    total = 0
    letters = []

    for line in lines:
        if line == '\n':
            total += len(letters)
            letters = []
        else:
            line = line.rstrip()
            for c in line:
                if c not in letters:
                    letters.append(c)

    return total + len(letters)


def solve_b():
    lines = get_input()

    total = 0
    letters = list(map(chr, range(97, 123)))

    for line in lines:
        if line == '\n':
            total += len(letters)
            letters = list(map(chr, range(97, 123)))
        else:
            line = line.rstrip()
            nletters = []

            for c in letters:
                if c in line:
                    nletters.append(c)
            letters = nletters

    return total + len(letters)


print(solve_a())

print(solve_b())
