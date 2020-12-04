def get_input():
    with open("input_day3.txt", 'r') as input_file:
        lines = input_file.readlines()
    return lines


def solve_a():
    return get_trees(1, 3)


def solve_b():
    a = get_trees(1, 1)
    b = get_trees(1, 3)
    c = get_trees(1, 5)
    d = get_trees(1, 7)
    e = get_trees(2, 1)

    return a * b * c * d * e


def get_trees(ymove, xmove):
    fmap = get_input()
    width = len(fmap[0]) - 1

    ypos = 0
    xpos = 0
    tree = 0

    while ypos < len(fmap) - 1:
        ypos += ymove
        xpos = (xpos + xmove) % width

        if fmap[ypos][xpos] == '#':
            tree += 1

    return tree


print(solve_a())
print(solve_b()
