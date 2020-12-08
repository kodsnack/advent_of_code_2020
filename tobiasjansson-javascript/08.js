const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');
let program = [];
let pc = 0;

lines.forEach(line => {
    let parsed = line.match(/([a-z]+) ([-+]\d+)/);
    let inst = parsed[1];
    let value = parseInt(parsed[2]);
    program[pc++] = { inst: inst, value: value, exec: false }
});

// Del 1
var res = exec(program);
if (res.infite) {
    console.log('Infinite loop, acc =', res.acc);
}

// Del 2
for (var i = 0; i < program.length; i++) {
    // nop to jmp
    var res = exec(change(i, program, true, false));
    if (!res.infite) {
        console.log('ntoj Exited, acc =', res.acc);
    }
    // jmp to nop
    res = exec(change(i, program, false, true));
    if (!res.infite) {
        console.log('jton Exited, acc =', res.acc);
    }
}

function exec(orgprogram) {
    pc = 0;
    let program = JSON.parse(JSON.stringify(orgprogram));
    let loop = false;
    let acc = 0;
    while (pc < program.length && !loop) {
        let inst = program[pc].inst;
        let value = program[pc].value;
        program[pc].exec = true;
        switch (inst) {
            case 'nop': break;
            case 'acc': acc += value; break;
            case 'jmp':
                if (pc + value >= program.length) {
                    // Vi försöker hoppa ut, avsluta
                    return { acc: acc, infite: false };
                }
                if (program[pc + value].exec == true) {
                    loop = true;
                }
                pc += value;
                continue;
            default: break;
        }
        pc++;
    }
    return { acc: acc, infite: loop }
}

function change(pc, orgprogram, noptojmp, jmptonop) {
    let program = JSON.parse(JSON.stringify(orgprogram));
    if (noptojmp && program[pc].inst == 'nop') {
        program[pc].inst = 'jmp';
    }
    if (jmptonop && program[pc].inst == 'jmp') {
        program[pc].inst = 'nop';
    }

    return program;
}