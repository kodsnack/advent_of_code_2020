def solve(lines):
    trees = 0
    frees = 0
    nones = 0
    for i in range(len(lines)):
        pos = (3*i) % (len(lines[0]) -1)
        if lines[i][pos] == '#':
            trees += 1
        elif lines[i][pos] == '.':
            frees += 1
        else:
            nones += 1
    return (trees, frees, nones)

if __name__ == '__main__':
    lines = []
    with open('3.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
