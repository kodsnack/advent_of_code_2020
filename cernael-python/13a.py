def solve(lines):
    now = int(lines[0].strip())
    buses = [int(x) for x in lines[1].split(',') if x != 'x']

#    now, buses = 939, [7,13,59,31,19]

    deps = [[b,(1+int(now / b)) * b-now] for b in buses]

    ans = [0, 2*now]
    for i in range(len(deps)):
        if deps[i][1] < ans[1]: ans = deps[i]
    return  ans[0] * ans [1]

if __name__ == '__main__':
    lines = []
    with open('13.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
