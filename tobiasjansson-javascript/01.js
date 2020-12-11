const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n').map(x => parseInt(x));

// Min funktion
console.log(hittatre(lines));

// Tips från AoC att använda 3SUM
console.log(sum3(lines));

function hittatre(data) {
    for (var i = 0; i < data.length; i++) {
        for (var k = 0; k < data.length; k++) {
            if (k != i) {
                for (var j = 0; j < data.length; j++) {
                    if (i != j && k != j) {
                        let v1 = data[i];
                        let v2 = data[j];
                        let v3 = data[k];
                        if (v1 + v2 + v3 == 2020) {
                            return v1 * v2 * v3;
                        }
                    }
                }
            }
        }
    }
}

// vill lära mig något
// https://en.wikipedia.org/wiki/3SUM

function sum3(data) {
    let S = data.sort();
    let n = data.length;
    let a, b, c, start, end;
    for(var i=0;i < n - 2 ; i++) {
        a = S[i];
        start = i + 1;
        end = n - 1;
        while(start < end) {
            b = S[start];
            c = S[end];
            let sum = Math.round((a - (2020/3)) + (b - (2020/3)) + (c - (2020/3)));
            if(sum == 0) {
                return a*b*c;
            } else if(sum > 0 ) {
                end -= 1;
            } else {
                start += 1;
            }
        }
    }
}
