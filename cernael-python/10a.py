def solve(lines):
    lines.append('0')
    s =  map(lambda a: int(a.strip()), lines)
    ss = sorted(s)
    threes = 1
    ones = 0

    for i in range(len(ss) - 1):
        if ss[i+1] - ss[i] == 1:
            ones += 1
        elif ss[i+1] - ss[i] == 3:
            threes += 1
    return (threes , ones, threes * ones)


if __name__ == '__main__':
    lines = []
    with open('10.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
