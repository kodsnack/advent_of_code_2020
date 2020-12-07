def solve(lines):

    group = []
    answer = ''
    answers = 'abcdefghijklmnopqrstuvwxyz'
    for i in range(len(lines)):
        if lines[i].strip() == '':
            group.append(len(answer))
            answer = ''
        else:
            for f in range(26):
                if answers[f] in lines[i] and answers[f] not in answer:
                    answer += answers[f]
                    
    group.append(len(answer))

#    return group
    return sum(group, 0)

if __name__ == '__main__':
    lines = []
    with open('6.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
