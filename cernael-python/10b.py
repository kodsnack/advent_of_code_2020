def solve(lines):
    lines.append('0')
    s =  map(lambda a: int(a.strip()), lines)

    ss = sorted(s)
    ss.append(max(ss)+3)
    ones = 0
    stretches = []

    for i in range(len(ss) - 1):
        if ss[i+1] - ss[i] == 1:
            ones += 1
        elif ss[i+1] - ss[i] == 2:
            return "ERROR"
        elif ss[i+1] - ss[i] == 3:
            stretches.append(ones)
            ones = 0
    prod = 1
    for i in range(len(stretches)):
        if stretches[i] == 0:
            prod *= 1
        elif stretches[i] == 1:
            prod *= 1
        elif stretches[i] == 2:
            prod *= 2
        elif stretches[i] == 3:
            prod *= 4
        elif stretches[i] == 4:
            prod *= 7
    return prod

if __name__ == '__main__':
    lines = []
    with open('10.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
