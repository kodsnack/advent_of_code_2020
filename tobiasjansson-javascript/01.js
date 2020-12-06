const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

for (var i = 0; i < lines.length; i++) {
    for (var k = 0; k < lines.length; k++) {
        if (k != i) {
            for (var j = 0; j < lines.length; j++) {
                if (i != j && k != j) {
                    let v1 = lines[i];
                    let v2 = lines[j];
                    let v3 = lines[k];
                    if (parseInt(v1) + parseInt(v2) + parseInt(v3) == 2020) {
                        console.log(v1, v2, v3, v1 * v2 * v3);
                        return;
                    }
                }
            }
        }
    }
};
