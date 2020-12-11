const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

// Del 1
let nonvalid = preambleit(lines, 25);

// Del 2
let range = rangesum(lines, nonvalid);
console.log(Math.min(...range) + Math.max(...range));

function preambleit(data, psize) {
    let i = 0;
    let isFound = false;
    while (i < data.length && !isFound) {
        line = data[i];
        if (line.length > 0) {
            num = parseInt(line);
            if (i >= psize) {
                let preamble = lines.slice(i - psize, i).map(x => parseInt(x));
                preamble['valid'] = findsum(num, preamble);
                if (!preamble['valid']) {
                    console.log('Not valid: ', num);
                    isFound = true;
                }
                preamble = [num];
            }
        }
        i++;
    };
    return num;
}

function rangesum(data, num) {
    let i = 0;
    let isFound = false;
    let range = [];
    while (i < data.length && !isFound) {
        line = data[i];
        if (line.length > 0) {
            for (var j = 0; j < i; j++) {
                range = lines.slice(j, i).map(x => parseInt(x));
                let sum = range.reduce((a, b) => a + b, 0);
                if (sum == num) {
                    isFound = true;
                    break;
                }
                if (sum > num) {
                    continue;
                }
            }
        }
        i++;
    }
    return range;
}

function findsum(sum, preamble) {
    for (var i = 0; i < preamble.length; i++) {
        for (var j = i; j < preamble.length; j++) {
            let v1 = preamble[i];
            let v2 = preamble[j];
            if (parseInt(v1) + parseInt(v2) == sum) {
                return true;
            }
        }
    }
    return false;
}
