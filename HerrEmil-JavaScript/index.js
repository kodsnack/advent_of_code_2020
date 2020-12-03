const readInput = (day) =>
  require("fs")
    .readFileSync(`./day-${("" + day).padStart(2, "0")}-input.txt`)
    .toString();

const {
  findPairWithSum2020,
  findTripleWithSum2020,
  parseDay01Data,
} = require("./day-01");
const day01Data = parseDay01Data(readInput(1));
day01Puzzle1 = findPairWithSum2020(day01Data);
day01Puzzle2 = findTripleWithSum2020(day01Data);
console.log(`Day 01, puzzle 1 answer: ${day01Puzzle1}`); // 1019371
console.log(`Day 01, puzzle 2 answer: ${day01Puzzle2}`); // 278064990

const {
  isCharAtExclusivelyOnePosition,
  isCharCountWithinRange,
  parseDay02ListItem,
} = require("./day-02");
const day02Data = readInput(2).split(/\r?\n/).map(parseDay02ListItem);
const day02Puzzle1 = day02Data.filter(isCharCountWithinRange).length;
const day02Puzzle2 = day02Data.filter(isCharAtExclusivelyOnePosition).length;
console.log(`Day 02, puzzle 1 answer: ${day02Puzzle1}`); // 424
console.log(`Day 02, puzzle 2 answer: ${day02Puzzle2}`); // 747

const { parseDay03Data, solve1, solve2 } = require("./day-03");
const day03Data = parseDay03Data(readInput(3));
const day03Puzzle1 = solve1(day03Data, 3, 1);
const day03Puzzle2 = solve2(day03Data, [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
]);
console.log(`Day 03, puzzle 1 answer: ${day03Puzzle1}`); // 159
console.log(`Day 03, puzzle 2 answer: ${day03Puzzle2}`); // 6419669520

const { parseDay04Data, solve1, solve2 } = require("./day-04");
const day04Data = parseDay04Data(readInput(4));
const day04Puzzle1 = solve1(day04Data);
const day04Puzzle2 = solve2(day04Data);
console.log(`Day 04, puzzle 1 answer: ${day04Puzzle1}`); //
console.log(`Day 04, puzzle 2 answer: ${day04Puzzle2}`); //
