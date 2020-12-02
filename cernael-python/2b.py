def solve(lines):
    res = []
    for p in lines:
        l = p.split(":")
        pol = l[0].split(" ")
        pw = l[1].strip()
        num = pol[0].split("-")
        first = pw[int(num[0])-1] == pol[1]
        last = pw[int(num[1])-1] == pol[1]
        if first != last: res.append(pw)
    return len(res)
if __name__ == '__main__':
    lines = []

    with open('2.txt') as f:
        for line in f.readlines():
            lines.append(line)

    print(solve(lines))
