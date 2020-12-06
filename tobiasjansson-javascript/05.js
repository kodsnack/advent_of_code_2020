const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

// Test
decodeSeat('FBFBBFFRLR');
decodeSeat('BFFFBBFRRR');
decodeSeat('FFFBBBFRRR');
decodeSeat('BBFFBBFRLL');

var max = 0;
var min = 1000;
var flight = [];
lines.forEach(seat => {
    var id = decodeSeat(seat);
    min = Math.min(min, id);
    max = Math.max(max, id);
    flight[id] = true;
});

// Del 1
console.log('Max seat: ' + max);

// Del 2
for (var i = min; i < max; i++) {
    if (!flight[i]) console.log(i);
}

function decodeSeat(seat) {
    var row = decode(seat.substr(0,7), 128);
    var col = decode(seat.substr(7,3), 8);
    var id = row * 8 + col;
    return id;
}

function decode(seat, size) {
    let pointer = size - 1;
    seat.split('').forEach(l => {
        switch (l) {
            case 'F':
            case 'L':
                size = size >> 1;
                pointer -= size;
                break;
            case 'B':
            case 'R':
                size = size >> 1;
                break;
        }
    });
    return pointer;
}
