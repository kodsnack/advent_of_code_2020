def solve(lines):
    res = []
    for p in lines:
        l = p.split(":")
        pol = l[0].split(" ")
        pw = l[1].strip()
        num = pol[0].split("-")
        if int(num[0]) <= pw.count(pol[1]) <= int(num[1]): res.append(pw)

    return len(res)
if __name__ == '__main__':
    lines = []

    with open('2.txt') as f:
        for line in f.readlines():
            lines.append(line)

    print(solve(lines))
