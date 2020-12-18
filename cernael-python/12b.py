def solve(lines):
    x = 0
    y = 0
    wpx = -10
    wpy = 1

#    lines = ['F10','N3','F7','R90','F11']
    for n in range(len(lines)):
        ins = lines[n][0]
        val = int(lines[n][1:])
        if ins == 'F':
            x += wpx * val
            y += wpy * val
        elif ins == 'R':
            for i in range(int(val/90)):
                wpx, wpy = -wpy, wpx
        elif ins == 'L':
            for i in range(int(val/90)):
                wpx, wpy = wpy, -wpx
        elif ins == 'N':
            wpy += val
        elif ins == 'S':
            wpy -= val
        elif ins == 'E':
            wpx -= val
        elif ins == 'W':
            wpx += val
 #       print(lines[n], x, y, wpx, wpy)

    return abs(x) + abs(y)

if __name__ == '__main__':
    lines = []
    with open('12.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
