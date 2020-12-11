// Testar med ett eget utilbibliotek
const util = require('./util.js');

let data = util.getInputTrim().map(x => parseInt(x)).sort((a, b) => a - b);
// Sätt in start och mål
data.unshift(0);
data.push(data[data.length - 1] + 3);

// Del 1
console.log(findsteps(data));
// Del 2
console.log(findpaths(data));

function findsteps(sorted) {
    let end = 0;
    let onestep = 0;
    let threestep = 0;
    for (var i = 0; i < sorted.length; i++) {
        let jolt = sorted[i];
        if (end <= jolt + 3) {
            if (jolt - end == 1) onestep++;
            if (jolt - end == 3) threestep++;
            end = jolt;
        }
    }
    return onestep * threestep;
}

// Hjälpte mig fatta
// https://www.geeksforgeeks.org/dynamic-programming/
function findpaths(data) {
    let chaincount = data.map(x => 0);
    chaincount[0] = 1;
    let chains = [];
    for (var i = 0; i < data.length; i++) {
        for (var j = 0; j < i; j++) {
            if (data[i] - data[j] <= 3) {
                chaincount[i] += chaincount[j];

                if(chains[data[i]] == undefined) chains[data[i]] = [];
                chains[data[i]].push(data[j]);
            }
        }
    }
    graphvizprint(chains);
    return chaincount[data.length - 1];
}

// Jag behövde det här för att fatta
function graphvizprint(data) {
    console.log('digraph G {');
    console.log('rankdir="LR"');
    Object.keys(data).forEach(d => {
        data[d].forEach(to => {
            console.log(to + '->' + d + ';');
        });
    })
    console.log('}');
}