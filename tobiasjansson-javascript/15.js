let test1 = [0, 3, 6]; // 0
let test2 = [3, 2, 1]; // 438
let test3 = [3, 1, 2]; // 1836

let input = [20, 9, 11, 0, 1, 2];

let numbers = input;

let lastspoken = [];
let last = 0;
const max = 30000000;
//const max = 2020;
for (var turn = 0; turn < max; turn++) {
    if (turn < numbers.length) {
        last = numbers[turn];
        lastspoken[last] = { turn: turn + 1 };
    } else {
        if (lastspoken[last] == undefined || lastspoken[last].turnprev == undefined) {
            last = 0;
            if(lastspoken[0] == undefined) lastspoken[0] = {turn:0};
            lastspoken[0] = { turn: turn + 1, turnprev: lastspoken[0].turn };
        } else {
            last = lastspoken[last].turn - lastspoken[last].turnprev;
            if (lastspoken[last] == undefined) lastspoken[last] = {};
            lastspoken[last] = { turn: turn + 1, turnprev: lastspoken[last].turn};
        }
    }
}
console.log('Turn:', turn, 'Saying', last)
