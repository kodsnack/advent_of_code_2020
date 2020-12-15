def solve(lines):
    mem = {}
    ones, nils = [0],[0]
    masks = [0]
    for l in range(len(lines)):
        line = lines[l].split(' = ')
        if line[0] == 'mask':
            ones, nils = [0],[0]
            masks = [[0,0]]
            for c in range(len(line[1])):
                if line[1][c] == '1':
                    masks = [[x[0] + 2**(35-c), x[1]] for x in masks]
                elif line[1][c] == 'X':
                    newones = [[x[0] + 2**(35-c), x[1]] for x in masks]
                    newnils = [[x[0], x[1] + 2**(35-c)] for x in masks]
                    masks = newones + newnils

        elif line[0][:3] == 'mem':
            for i in range(len(masks)):
                addr = int(line[0][4:-1])
                addr = addr | masks[i][0]
                addr = addr & ~masks[i][1]
                mem[addr] = int(line[1])
    return sum(mem.values())



if __name__ == '__main__':
    lines = []
    with open('14.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
