def solve(lines):
    trees = 1
    for i in [(1,1),(3,1),(5,1),(7,1),(1,2)]:
#    for i in [(1,2)]:
        treenums = solveslope(lines, i[0], i[1])
        print(i, treenums)
        trees *= treenums 
    return trees

def solveslope(lines, slopex, slopey):
    trees = 0
    for i in range(0, len(lines), slopey):
        pos = int(slopex*i/slopey) % (len(lines[0]) -1)
#        print(pos*' '+'v')
 #       print(lines[i])
        if lines[i][pos] == '#':
            trees += 1
    return trees

if __name__ == '__main__':
    lines = []
    with open('3.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
