def solve(lines):
    buses = [x for x in lines[1].split(',')]

#    now, buses = 939, [7,13,'x','x',59,'x',31,19]
    # [[offset, busID]]
    deps = [[b,int(buses[b])] for b in range(len(buses)) if buses[b] != 'x']

    t = 0
    per = 1
    for i in range(len(deps)):
        while (t + deps[i][0]) % deps[i][1] != 0:
            t += per
        per *= deps[i][1] # works if all IDs are prime
#    for i in range(len(deps)):
 #       deps[i].append(int(per/deps[i][1]))

    return  t

if __name__ == '__main__':
    lines = []
    with open('13.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
