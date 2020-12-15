// Testar med ett eget utilbibliotek
const util = require('./util.js');

let data = util.getInputTrim();

let earliest = data[0];
let notes = data[1].split(',');

// Test notes
//notes = '67,7,59,61'.split(',');
//notes = '67,x,7,59,61'.split(',');
//notes = '67,7,x,59,61'.split(',');
//notes = '1789,37,47,1889'.split(',');

let buses = notes.filter(note => {
    if (note != 'x') return parseInt(note)
    else return 'x';
});
console.log(earliest);

// Del 1
findnext(earliest);

// Del 2
// 103345759400000
contest(1068774);
//contest(0);

function findnext(time) {
    let nextbus = 0;
    let nexttime = 0;
    while (nexttime == 0) {
        buses.forEach(bus => {
            if (calcbus(time, bus)) {
                nextbus = bus;
                nexttime = time;
            }
        })
        time++;
    }
    console.log((nexttime - earliest) * nextbus);
    return nextbus, nexttime;
}

function contest(time) {
    let found = false;
    let bus0 = parseInt(buses[0]);
    // let busIDs = buses.filter(x => x != 'x').map(x => parseInt(x));
    // let busMax = Math.max(...busIDs);
    // let busMaxIndex = buses.indexOf(busMax+''); // Back to string for match
    // console.log(busMax, busMaxIndex);
    while (!found) {
        if(time % (10000000) == 0) console.log(time);
            let onnext = true;
            let skip = 1;
            //for(var i=buses.length; i > 0 && onnext; i--) {
            for(var i = 0; i < buses.length && onnext; i++) {
                let bus = buses[i];
                if (bus != 'x') {
                    if (!calcbus(time + i, bus)) {
                        //console.log(time+i);
                        onnext = false;
                        //skip = parseInt(bus) - i - 1;
                        skip = bus0 + 
                        //console.log(skip);
                        //onnext = true;
                    }
                }
            }
            if (onnext) {
                console.log('At minute', time);
                found = onnext;
            }
            time+=skip;
    }
}

function calcbus(time, id) {
    if (id == 'x') return false;
    return time % id == 0;
}

function print(start) {
    let str = ''
    for (var time = start; time < start + buses.length + 2; time++) {
        str = '';
        for (let bi = 0; bi < buses.length; bi++) {
            str += calcbus(time, bi) ? 'D' : '.' + ' ';
        }
        console.log(time, str);
    }
}