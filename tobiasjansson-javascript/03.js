const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

const width = lines[0].length;
const height = lines.length;

function trees(dx, dy) {
    let x = 0;
    let y = 0;
    let tree = 0;
    while (y < height) {
        let line = lines[y].split('');
        if (line[x % width] == '#') tree++;
        x += dx;
        y += dy;
    }
    return tree;
}

// Del 1
console.log(trees(3,1));

// Del 2
console.log(trees(1,1) * trees(3,1) * trees(5,1) * trees(7,1) * trees(1,2));