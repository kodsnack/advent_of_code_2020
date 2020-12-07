const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

let groups = [];
let group = {};
group.num = 0;
group.answers = {};
lines.forEach(line => {
    if(line.length == 0) {
        groups.push(group);
        group = {};
        group.answers = {};
        group.num = 0;
    } else group.num++;
    line.split('').forEach(l =>  {
        if(group.answers[l] == undefined) group.answers[l] = 0;
        group.answers[l] = group.answers[l] + 1;
    });  
});
groups.push(group);

let sum = 0;
groups.forEach(g => {
    let a = Object.values(g.answers).filter(x => x == g.num).length;
    sum += a;
});
console.log(sum);