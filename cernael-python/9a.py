import functools
def solve(lines):
    cipherlen = 25
#    lines = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
    valids = []
    for i in range(len(lines)):
        num = lines[i].strip()
        valids.append([num, [int(num) + int(a[0]) for a in valids]])
        if len(valids) > cipherlen:
            reduced_valids = functools.reduce(lambda a,b: a+b, [l[1] for l in valids[1:]])
            print('rv',reduced_valids)
            if int(num) not in reduced_valids:
                return num
            valids.pop(0)
            for j in range(cipherlen):
                valids[j][1].pop(0)



        print('v',valids)
    return 1

if __name__ == '__main__':
    lines = []
    with open('9.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
