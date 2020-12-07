const readInput = (day) =>
  require("fs")
    .readFileSync(`./day-${("" + day).padStart(2, "0")}-input.txt`)
    .toString();

const day01 = require("./day-01");
const day01Data = day01.parseData(readInput(01));
console.log(`Day 01, puzzle 1 answer: ${day01.solve1(day01Data)}`); // 1019371
console.log(`Day 01, puzzle 2 answer: ${day01.solve2(day01Data)}`); // 278064990

const day02 = require("./day-02");
const day02Data = day02.parseData(readInput(02));
console.log(`Day 02, puzzle 1 answer: ${day02.solve1(day02Data)}`); // 424
console.log(`Day 02, puzzle 2 answer: ${day02.solve2(day02Data)}`); // 747

const day03 = require("./day-03");
const day03Data = day03.parseData(readInput(03));
console.log(`Day 03, puzzle 1 answer: ${day03.solve1(day03Data)}`); // 159
console.log(`Day 03, puzzle 2 answer: ${day03.solve2(day03Data)}`); // 6419669520

const day04 = require("./day-04");
const day04Data = day04.parseData(readInput(04));
console.log(`Day 04, puzzle 1 answer: ${day04.solve1(day04Data)}`); // 256
console.log(`Day 04, puzzle 2 answer: ${day04.solve2(day04Data)}`); // 198

const day05 = require("./day-05");
const day05Data = day05.parseData(readInput(05));
console.log(`Day 05, puzzle 1 answer: ${day05.solve1(day05Data)}`); // 883
console.log(`Day 05, puzzle 2 answer: ${day05.solve2(day05Data)}`); // 532

const day06 = require("./day-06");
const day06Data = day06.parseData(readInput(06));
console.log(`Day 06, puzzle 1 answer: ${day06.solve1(day06Data)}`); // 6542
console.log(`Day 06, puzzle 2 answer: ${day06.solve2(day06Data)}`); // 3299

const day07 = require("./day-07");
const day07Data = day07.parseData(readInput(07));
console.log(`Day 07, puzzle 1 answer: ${day07.solve1(day07Data)}`); //
console.log(`Day 07, puzzle 2 answer: ${day07.solve2(day07Data)}`); //
