def solve(lines):

    group = []
    answer = ''
#    for i in range(10):
    new = True
    for i in range(len(lines)):
        if lines[i].strip() == '':
#            group.append(answer)
            group.append(len(answer))
            answer = ''
            new = True
        else:
            if new:
                answer = lines[i].strip()
                new = False
                continue
            ret = ''
            for n in range(len(answer)):
                if answer[n] in lines[i]:
                    ret += answer[n]
            answer = ret
                    
#    group.append(answer)
    group.append(len(answer))

#    return group
    return sum(group, 0)

if __name__ == '__main__':
    lines = []
    with open('6.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
