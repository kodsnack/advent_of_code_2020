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
//console.table(bags);
let totalbags = 0;
let count = 0;
Object.keys(bags).forEach(b => {
    count = 0;
    countshiny(count, b);
    if(count > 0) {
        totalbags++;
    }
});
console.log(totalbags);

function subbags(top, subbags) {
    subbags.split(',').forEach(x => {
        let bag = x.match(/\s?(\d+) (.*) bag\.*/);
        if (bag != null) {
            let amount = bag[1];
            let contain = bag[2];
            if (bags[top] == undefined) {
                bags[top] = [];
            }
            bags[top].push(contain);
        }
    });
}

function countshiny(shiny, bag) {
    if (bags[bag]) {
        bags[bag].forEach(b => {
            if (b == 'shiny gold') count++;
            else countshiny(count, b);
        });
    }
    return shiny;
}