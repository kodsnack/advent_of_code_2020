def solve(lines):
    dir = 270
    x = 0
    y = 0
#    lines = ['F10','N3','F7','R90','F11']
    for n in range(len(lines)):
        ins = lines[n][0]
        val = int(lines[n][1:])
        if ins == 'F':
            if dir == 0:
                y += val
            elif dir == 90:
                x += val
            elif dir == 180:
                y -= val
            elif dir == 270:
                x -= val
        elif ins == 'R':
            dir = (dir - val) % 360
        elif ins == 'L':
            dir = (dir + val) % 360
        elif ins == 'N':
            y += val
        elif ins == 'S':
            y -= val
        elif ins == 'E':
            x -= val
        elif ins == 'W':
            x += val
#        print(lines[n], x, y, dir)

    return abs(x) + abs(y)

if __name__ == '__main__':
    lines = []
    with open('12.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
