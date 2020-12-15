def solve(lines):
    mem = {}

    ones, nils = 0,0
    for l in range(len(lines)):
        line = lines[l].split(' = ')
        if line[0] == 'mask':
            ones, nils = 0,0
            for c in range(len(line[1])):
                if line[1][c] == '1':
                    ones += 2**(35-c)
                elif line[1][c] == '0':
                    nils += 2**(35-c)

        elif line[0][:3] == 'mem':
            line[1] = int(line[1]) | ones
            line[1] = int(line[1]) & ~nils
            exec (line[0] + ' = ' +  str(line[1]))
    return sum(mem.values())



if __name__ == '__main__':
    lines = []
    with open('14.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
