const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

let valid = 0;
lines.forEach(line => {
    let policy = line.match(/(\d+)-(\d+) ([a-z]): (.*)/);

    let regex = new RegExp(policy[3],'g');
    let result = policy[4].match(regex);

    let num = result == null ? 0 : result.length;
    if(num >= policy[1] && num <= policy[2]) {
        valid++
    }
});

console.log('Num valid: ', valid);
