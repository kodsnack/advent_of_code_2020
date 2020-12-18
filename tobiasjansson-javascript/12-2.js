// Testar med ett eget utilbibliotek
const util = require('./util.js');

let data = util.getInputTrim();

let x = 0;
let y = 0;

[x, y] = spaceship([10, -1]);

console.log(manhattan(0, 0, x, y));

function spaceship(waypoint) {
    let x = 0;
    let y = 0;

    let wx = waypoint[0];
    let wy = waypoint[1];

    let wdx = wx - x;
    let wdy = wy - y;

    data.forEach(line => {
        let c,s,nwx,nwy;
        let p = line.match(/([A-Z])?(\d+).*/);
        let cmd = p[1].trim();
        let val = parseInt(p[2]);
        wdx = wx - x;
        wdy = wy - y;
        switch (cmd) {
            case 'N':
                wy -= val;
                break;
            case 'S':
                wy += val;
                break;
            case 'E':
                wx += val;
                break;
            case 'W':
                wx -= val;
                break;
            case 'L':
                [c,s] = degtodir(val);
                nwx = c * (wx-x) + s * (wy-y) + x;
                nwy = -s * (wx-x) + c * (wy-y) + y;
                wx = nwx;
                wy = nwy;
                break;
            case 'R':
                [c,s] = degtodir(val);
                nwx = c * (wx-x) - s * (wy-y) + x;
                nwy = s * (wx-x) + c * (wy-y) + y;
                wx = nwx;
                wy = nwy;
                break;
            case 'F':
                x += val * wdx;
                y += val * wdy;
                wx = x + wdx;
                wy = y + wdy;
                break;
            default:
                console.log('!!!!', cmd, val);
                break;
        }
        console.log('Ship:', Math.round(x), Math.round(y), 'WP:', wx, wy);
    });
    return [x, y];
}

function degtodir(degree) {
    var rad = (degree * Math.PI) / 180;
    var a = Math.round(Math.cos(rad),2);
    var b = Math.round(Math.sin(rad),2);
    return [a, b];
}

function manhattan(sx, sy, x, y) {
    return Math.abs(sx - x) + Math.abs(sy - y);
}