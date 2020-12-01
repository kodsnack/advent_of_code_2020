def solve(lines):
    ints = [int(x) for x in lines]
    sol = [(x, y, x+y, x*y) for x in ints for y in ints if x+y == 2020]
    return str(sol[0])

if __name__ == '__main__':
    lines = []

    with open('1.txt') as f:
        for line in f.readlines():
            lines.append(line)

    print(solve(lines))
