prg = []
with open('input.txt','r') as f:
    for line in f:
        op_code, value = line.strip().split(' ')
        prg.append((op_code, int(value), False))

instr_map = {
    "acc": lambda x: (x, 1),
    "jmp": lambda x: (0, x),
    "nop": lambda x: (0, 1)
}

acc = 0
instr_ptr = 0
while not prg[instr_ptr][2]:
    (op_code, value, _) = prg[instr_ptr]
    prg[instr_ptr] = (op_code, value, True)
    acc_chg, next_instr = instr_map[op_code](value)
    acc += acc_chg
    instr_ptr += next_instr
print("Total acc before revisit", acc)

last_occurance = -1
while instr_ptr < len(prg):
    prg_copy = []
    current_occurance = -1
    for idx, (op_code, value, _) in enumerate(prg):
        if idx > last_occurance and current_occurance == -1 and op_code in ['nop', 'jmp']:
            op_code = "nop" if op_code == "jmp" else "jmp"
            current_occurance = idx
        prg_copy.append((op_code, value, False))
    last_occurance = current_occurance

    acc = 0
    instr_ptr = 0
    while instr_ptr < len(prg_copy) and not prg_copy[instr_ptr][2]:
        (op_code, value, _) = prg_copy[instr_ptr]
        prg_copy[instr_ptr] = (op_code, value, True)
        acc_chg, next_instr = instr_map[op_code](value)
        acc += acc_chg
        instr_ptr += next_instr
print("Changed instruction on line", last_occurance + 1, ". Total acc is", acc)
