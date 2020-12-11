const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

let bags = [];
lines.forEach(line => {
    let bag = line.match(/(.*) bags contain (.*)/);
    let top = bag[1];
    let contain = bag[2];

    subbags(top, contain);
});

console.log(countbags(0, 'shiny gold'));

function subbags(top, subbags) {
    subbags.split(',').forEach(x => {
        let bag = x.match(/\s?(\d+) (.*) bag\.*/);
        if (bags[top] == undefined) {
            bags[top] = [];
        }
        if (bag != null) {
            let amount = bag[1];
            let contain = bag[2];
            bags[top].push({color:contain, amount:parseInt(amount)});
        } else {
            bags[top].push({amount:0});           
        }
    });
}

function countbags(count, bag) {
    if (bags[bag]) {
        bags[bag].forEach(b => {
           count += b.amount;
           count += b.amount * countbags(0, b.color);
        });
    }
    return count;
}