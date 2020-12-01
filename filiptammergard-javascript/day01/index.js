const fs = require("fs");

const expenses = fs.readFileSync("day01/input.txt").toString().split("\n");

function computePartOne() {
  let answer;
  expenses.every((expense) => {
    return expenses.every((expense2) => {
      if (Number(expense) + Number(expense2) === 2020) {
        answer = Number(expense) * Number(expense2);
        return false;
      } else return true;
    });
  });
  return answer;
}

function computePartTwo() {
  let answer;
  expenses.every((expense) => {
    return expenses.every((expense2) => {
      return expenses.every((expense3) => {
        if (Number(expense) + Number(expense2) + Number(expense3) === 2020) {
          answer = Number(expense) * Number(expense2) * Number(expense3);
          return false;
        } else return true;
      });
    });
  });
  return answer;
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
