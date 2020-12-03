const fs = require('fs');
const { platform } = require('os');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

let valid = 0;
lines.forEach(line => {
    let policy = line.match(/(\d+)-(\d+) ([a-z]): (.*)/);  
    const p1i = parseInt(policy[1]) - 1; // Toboggan arrays DONT start at zero
    const p2i = parseInt(policy[2]) - 1;

    const p1v = policy[4][p1i] == policy[3];
    const p2v = policy[4][p2i] == policy[3];

    //console.log(p1i, p2i, policy[4][p1i],policy[4][p2i], p1v, p2v);

    if( (p1v || p2v ) && !(p1v && p2v)) {
        valid++;
    }
});

console.log('Num valid: ' + valid);
