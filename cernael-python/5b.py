def solve(lines):
    seats = []
    for line in range(len(lines)):
        r, c, i = 0, 0, 0
        for l in lines[line]:
            if l in 'BF':
               r *= 2
               r += l=='B'
            elif l in 'RL':
               c *= 2
               c += l=='R'
        i = r*8+c
        seats.append(i)
    seats.sort()
    return seats[560:600]

if __name__ == '__main__':
    lines = []
    with open('5.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
