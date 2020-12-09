const fs = require("fs");

const input = fs.readFileSync("day09/input.txt").toString().split("\n");

function computePartOne() {
  const numbers = parseInput(input);
  const invalidNumber = getInvalidNumber(numbers);
  return invalidNumber;
}

function computePartTwo() {
  const numbers = parseInput(input);
  const invalidNumber = getInvalidNumber(numbers);
  const encryptionWeakness = getEncryptionWeakness({ numbers, invalidNumber });
  return encryptionWeakness;
}

function parseInput(input) {
  return input.map((row) => parseInt(row));
}

function getInvalidNumber(numbers) {
  const preambleSize = 25;
  for (let i = preambleSize; i < numbers.length; i++) {
    const previousPreamble = numbers.slice(i - preambleSize, i);
    const sums = new Set();
    previousPreamble.forEach((number) => {
      previousPreamble.forEach((number2) => {
        sums.add(number + number2);
      });
    });
    const valid = [...sums].includes(numbers[i]);
    if (!valid) {
      return numbers[i];
    }
  }
}

function getEncryptionWeakness({ invalidNumber, numbers }) {
  for (let i = 2; i < numbers.length; i++) {
    for (let j = 0; j < numbers.length; j++) {
      const contiguousSet = numbers.slice(j, j + i);
      const sum = contiguousSet.reduce((acc, curr) => acc + curr, 0);
      if (sum === invalidNumber) {
        const encryptionWeakness =
          Math.min(...contiguousSet) + Math.max(...contiguousSet);
        return encryptionWeakness;
      }
    }
  }
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
