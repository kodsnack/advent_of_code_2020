const fs = require('fs');
const args = process.argv.slice(2);

function getInput() {
    const data = fs.readFileSync(args[0], 'utf8');

    let lines = data.split('\n');

    return lines;
}

// Tar bort tomrader
function getInputTrim() {
    return getInput().filter(x => x.length > 0);
}

function astar(data) {

}

function manhattan(a, b) {

}

exports.getInput = getInput;
exports.getInputTrim = getInputTrim;
exports.astar = astar;
exports.manhattan = manhattan;