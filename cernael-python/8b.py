import copy

def solve(lines):
    code = [x.split() for x in lines]
    for n in range(len(lines)):
        if code[n][0] == 'acc':
            continue
        res = test(copy.deepcopy(code), n)
        if res[0] == 'term': return res

    return code[-20:]

def test(code, line):
    if code[line][0] == 'nop':
        code[line][0] = 'jmp'
    elif code[line][0] == 'jmp':
        code[line][0] = 'nop'
    return run(code)

def run(code):
    acc = 0
    pnt = 0

    while True:
        if pnt == len(code): return ['term', acc]
        elif len(code[pnt]) == 3:
            return ['loop', acc]
        code[pnt].append('vis')
        if code[pnt][0] == 'nop': pnt += 1
        elif code[pnt][0] == 'acc':
            acc += int(code[pnt][1])
            pnt += 1
        elif code[pnt][0] == 'jmp':
            pnt += int(code[pnt][1])

if __name__ == '__main__':
    lines = []
    with open('8.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
