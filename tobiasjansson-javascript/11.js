// Testar med ett eget utilbibliotek
const { platform } = require('os');
const util = require('./util.js');

let data = util.getInputTrim();
let seats = [];
data.forEach(d => seats.push(d.trim().split('')));

// Del 1
model(del1rules);
console.log(countOccupied());

// Reset for del 2
seats = [];
data.forEach(d => seats.push(d.trim().split('')));

// Del 2
model(del2rules);
console.log(countOccupied());

function model(ruleset) {
    let seatsChanged = 0;
    do {
        let newmodel = [];
        seatsChanged = 0;
        for (var y = 0; y < seats.length; y++) {
            for (var x = 0; x < seats[0].length; x++) {
                if (newmodel[y] == undefined) newmodel[y] = [];
                newmodel[y][x] = ruleset(x, y);
                if (get(x, y) != newmodel[y][x]) seatsChanged++;
            }
        }
        seats = JSON.parse(JSON.stringify(newmodel));
    } while (seatsChanged > 0)
}

function del1rules(x, y) {
    let nb = getNeigbours(x, y);
    let occ = nb.filter(x => x == '#').length;
    let seat = get(x, y);
    if (seat == 'L' && occ == 0) {
        return '#';
    } else if (seat == '#' && occ >= 4) {
        return 'L';
    }
    return seat;
}

function del2rules(x, y) {
    let occ = getLineOcc(x,y, [0,-1]);
    occ +=    getLineOcc(x,y, [1,-1]);
    occ +=    getLineOcc(x,y, [1, 0]);    
    occ +=    getLineOcc(x,y, [1, 1]);
    occ +=    getLineOcc(x,y, [0, 1]);
    occ +=    getLineOcc(x,y, [-1,1]);
    occ +=    getLineOcc(x,y, [-1,0]);
    occ +=    getLineOcc(x,y, [-1,-1]);
    //console.log(occ);
    let seat = get(x, y);
    if (seat == 'L' && occ == 0) {
        return '#';
    } else if (seat == '#' && occ >= 5) {
        return 'L';
    }
    return seat;
}

function countOccupied() {
    let occ = 0;
    for (var y = 0; y < seats.length; y++) {
        occ += seats[y].filter(x => x == '#').length;
    }
    return occ;
}

function getLineOcc(x, y, delta) {
    let line = [];
    let row = y + delta[1];
    let seat = x + delta[0];
    let place = '';
    while (get(seat, row) == '.') {
        place = get(seat, row);
        line.push(place);
        seat += delta[0];
        row += delta[1];
    }
    line.push(get(seat, row));
    return line.filter(x => x == '#').length;
}

function getNeigbours(x, y) {
    return [
        get(x, y - 1),
        get(x + 1, y - 1),
        get(x + 1, y),
        get(x + 1, y + 1),

        get(x, y + 1),
        get(x - 1, y + 1),
        get(x - 1, y),
        get(x - 1, y - 1),
    ]
}

function get(x, y) {
    let r = seats[y];
    if (r == undefined) return 'W';

    let g = seats[y][x];
    return g != undefined ? g : 'W'
}

function print() {
    console.log();
    for (var y = 0; y < seats.length; y++) {
        console.log(seats[y].join(''));
    }
}