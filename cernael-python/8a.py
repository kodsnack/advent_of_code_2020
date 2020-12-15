def solve(lines):
    acc = 0
    pnt = 0
    code = [x.split() for x in lines]

    while True:
        if len(code[pnt]) == 3:
            return acc
        code[pnt].append('vis')
        if code[pnt][0] == 'nop': pnt += 1
        elif code[pnt][0] == 'acc':
            acc += int(code[pnt][1])
            pnt += 1
        elif code[pnt][0] == 'jmp':
            pnt += int(code[pnt][1])
                   

    return code[0:10]

if __name__ == '__main__':
    lines = []
    with open('8.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
