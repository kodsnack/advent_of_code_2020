// Testar med ett eget utilbibliotek
const util = require('./util.js');

let data = util.getInputTrim();


// Del 1. Räkna summan
let mem = decoderchip(data);
console.log(mem.reduce((summa, value) => summa + calc(value), 0));

// Del 2. Fucka runt med minnesaddresser
let preparsed = [];
mem = decoderchipv2(data);
console.log(mem.reduce((summa, value) => summa + calc(value), 0));

function decoderchip(data) {
    let mem = [];
    let bitmask = '';
    data.forEach(line => {
        if (line.startsWith('mask')) {
            bitmask = line.match(/.* = (.*)/)[1];
        } else {
            let parse = line.match(/mem\[(\d+)\] = (\d+).*/);
            let addr = parse[1];
            let pvalue = parse[2];
            let binval = dec2bin(parseInt(pvalue));
            let value = pad(binval, 36);

            if (mem[addr] == undefined) mem[addr] = pad(0, 36);
            mem[addr] = applymask(bitmask, value);
        }
    })
    return mem;
}

function decoderchipv2(data) {
    let mem = [];
    let bitmask = '';
    data.forEach((line, i) => {
        if (line.startsWith('mask')) {
            bitmask = line.match(/.* = (.*)/)[1];
            console.log(i, bitmask, bitmask.split('').filter(x => x == 'X').length);
        } else {
            let parse = line.match(/mem\[(\d+)\] = (\d+).*/);
            let paddr = parse[1];
            let pvalue = parse[2];
            let binval = dec2bin(parseInt(pvalue));
            let value = pad(binval, 36);

            let binaddr = dec2bin(parseInt(paddr));
            let floater = applymask(bitmask, pad(binaddr, 36), true);
            let floaters = floating([], floater);
            floaters.forEach(binaddr => {
                let addr = calc(binaddr);
                if (mem[addr] == undefined) mem[addr] = pad(0, 36);
                mem[addr] = value;
            })
        }
    })
    return mem;
}

function floating(list, value) {
    let v = JSON.parse(JSON.stringify(value)).split('');
    if (v.includes('X')) {
        // if (preparsed.includes(v.join(''))) {
        //     let p = preparsed(v.join(''));
        //     console.log('cachehit');
        //     p.forEach(el => {
        //         floating(list, el);
        //     });
        // } else {
           // console.log(preparsed);
            for (var i = 0; i < v.length; i++) {
                if (v[i] == 'X') {
                    let v2 = JSON.parse(JSON.stringify(value)).split('');
                    v2[i] = '0';
                    //preparsed[v2.join('')] = 
                    floating(list, v2.join(''));

                    let v3 = JSON.parse(JSON.stringify(value)).split('');
                    v3[i] = '1';
                    //preparsed[v3.join('')] = 
                    floating(list, v3.join(''));
                }
            }
        }
   // } else {
        //let addr = calc(v.join(''));
        let addr = v.join('');
        if (!list.includes(addr)) {
            //console.log(addr);
            list.push(addr);
        }
    //}
    return list;
}

function applymask(mask, value, memdecode = false) {
    let size = 36;
    let zeropad = pad(value, 36);
    result = '';
    for (var i = 0; i < size; i++) {
        switch (mask[i]) {
            case '0':
                if (memdecode) result += zeropad[i];
                else result += '0';
                break;
            case '1':
                result += '1';
                break;
            case 'X':
                if (memdecode) result += 'X';
                else result += zeropad[i];
                break;
        }
    }
    return result;
}

function calc(value) {
    let size = 36;
    let result = 0;
    for (var i = 0; i < size; i++) {
        let bit = size - i - 1;
        let shift = lshift(1, bit);
        switch (value[i]) {
            case '0':
                break;
            case '1':
                result += shift;
                break;
        }
    }
    return result;
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
