const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

let sum = 0;
let groups = [];
let group = [];
lines.forEach(line => {
    if(line.length == 0) {
        groups.push(group);
        group = [];
    }
    line.split('').forEach(l =>  {
        if(group[l] == undefined) sum++;
        group[l] = 'yes';
    });  
});
groups.push(group);

console.log(sum);