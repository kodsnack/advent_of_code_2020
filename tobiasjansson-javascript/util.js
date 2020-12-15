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

function pad(n, width, z) {
    z = z || '0';
    n = n + '';
    return n.length >= width ? n : new Array(width - n.length + 1).join(z) + n;
}

function lshift(num, bits) {
    return num * Math.pow(2, bits);
}

function dec2bin(dec) {
    return (dec >>> 0).toString(2);
}

function astar(data) {

}

function manhattan(a, b) {

}

exports.getInput = getInput;
exports.getInputTrim = getInputTrim;
exports.astar = astar;
exports.manhattan = manhattan;