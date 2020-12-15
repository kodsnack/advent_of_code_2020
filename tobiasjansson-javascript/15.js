let test1 = [0, 3, 6]; // 0
let test2 = [3, 2, 1]; // 438
let test3 = [3, 1, 2]; // 1836

let input = [20, 9, 11, 0, 1, 2];

let numbers = input;

let lastspoken = new Map(); // Använde Map istället för Array och gick från 12min till 11s...
let last = 0;
const max = 30000000;
//const max = 2020;
for (var turn = 0; turn < max; turn++) {
    if (turn < numbers.length) {
        last = numbers[turn];
        lastspoken.set(last, { turn: turn + 1 });
    } else {
        if (lastspoken.get(last) == undefined || lastspoken.get(last).turnprev == undefined) {
            last = 0;
            if(lastspoken.get(0) == undefined) lastspoken.set(0, {turn:0} );
            lastspoken.set(0, { turn: turn + 1, turnprev: lastspoken.get(0).turn } );
        } else {
            let lastsp = lastspoken.get(last);
            last = lastsp.turn - lastsp.turnprev;
            if (lastspoken.get(last) == undefined) lastspoken.set(last, {});
            lastspoken.set(last, { turn: turn + 1, turnprev: lastspoken.get(last).turn} );
        }
    }
}
console.log('Turn:', turn, 'Saying', last)
