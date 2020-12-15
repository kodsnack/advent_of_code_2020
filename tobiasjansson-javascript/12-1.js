// Testar med ett eget utilbibliotek
const util = require('./util.js');

let data = util.getInputTrim();

let x, y;
[x, y] = spaceship(0); // 0 är öster
console.log(manhattan(0, 0, x, y));

function spaceship(dir) {
    let x = 0;
    let y = 0;

    let dx, dy;

    [dx, dy] = degtodir(dir);
    data.forEach(line => {
        let p = line.match(/([A-Z])?(\d+).*/);
        let cmd = p[1].trim();
        let val = parseInt(p[2]);
        switch (cmd) {
            case 'F':
                x += val * dx;
                y += val * dy;
                break;
            case 'N':
                y -= val;
                break;
            case 'S':
                y += val;
                break;
            case 'E':
                x += val;
                break;
            case 'W':
                x -= val;
                break;
            case 'L':
                dir -= val;
                [dx, dy] = degtodir(dir);
                break;
            case 'R':
                dir += val;
                [dx, dy] = degtodir(dir);
                break;
            default:
                console.log('!!!!', cmd, val);
                break;
        }
    });
    return [x, y];
}

function degtodir(degree) {
    var rad = (degree * Math.PI) / 180;
    var a = Math.round(Math.cos(rad));
    var b = Math.round(Math.sin(rad));
    return [a, b];
}

function manhattan(sx, sy, x, y) {
    return Math.abs(sx - x) + Math.abs(sy - y);
}