
programl = []


def initialize():
    with open("input_day8.txt", 'r') as input_file:
        lines = input_file.readlines()
        for line in lines:
            instruction, nr = line.strip().split()
            programl.append((instruction, int(nr)))
        programl.append(('end', 0))


def run_program(program):
    accumulator = 0
    checked = []
    pc = 0
    finished = False

    while pc not in checked:

        pline = program[pc]
        #print(' pc = ' + str(pc) + ' instruction = ' + pline[0] + 'nr = ' + str(pline[1]))
        checked.append(pc)

        if pline[0] == 'nop':
            pc += 1
        elif pline[0] == 'acc':
            accumulator += pline[1]
            pc += 1
        elif pline[0] == 'jmp':
            pc += pline[1]
        else:
            finished = True

    return accumulator, finished


def make_tempprog():
    tempprog = []
    for line in programl:
        tempprog.append(list(line))
    return tempprog


def solve_a():

    return run_program(programl)


def solve_b():

    f_acc = None
    for i, line in enumerate(programl):
        #print('i = ' + str(i) + ' line[0] = ' + line[0] + ' line[1] = ' + str(line[1]))
        if line[0] == 'acc':
            continue

        tempprog = make_tempprog()
        if line[0] == 'nop':
            tempprog[i][0] = 'jmp'

        elif line[0] == 'jmp':
            tempprog[i][0] = 'nop'
        #print(programl)
        #print(tempprog)

        acc, fin = run_program(tempprog)
        if fin is True:
            f_acc = acc

    return f_acc


initialize()

# print(solve_a())

print(solve_b())
