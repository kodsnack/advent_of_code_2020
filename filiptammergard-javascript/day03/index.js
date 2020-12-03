const fs = require("fs");

const input = fs.readFileSync("day03/input.txt").toString().split("\n");

function computePartOne() {
  const numberOfTrees = getNumberOfTrees(1, 3);
  return numberOfTrees;
}

function computePartTwo() {
  const numberOfTrees = [
    getNumberOfTrees(1, 1),
    getNumberOfTrees(1, 3),
    getNumberOfTrees(1, 5),
    getNumberOfTrees(1, 7),
    getNumberOfTrees(2, 1),
  ];
  const multiplied = numberOfTrees.reduce((product, curr) => product * curr);
  return multiplied;
}

function getNumberOfTrees(downSlope, rightSlope) {
  const numberOfTrees = input.reduce((accTrees, currentLine, currentIndex) => {
    const currentLocationInPattern = getCurrentLocationInPattern(
      downSlope,
      rightSlope,
      currentLine,
      currentIndex
    );
    if (isTree(currentLine, currentLocationInPattern)) {
      return accTrees + 1;
    } else {
      return accTrees;
    }
  }, 0);
  return numberOfTrees;
}

function getCurrentLocation(downSlope, rightSlope, currentIndex) {
  return (currentIndex * rightSlope) / downSlope;
}

function getCurrentLocationInPattern(
  downSlope,
  rightSlope,
  currentLine,
  currentIndex
) {
  const currentLocation = getCurrentLocation(
    downSlope,
    rightSlope,
    currentIndex
  );
  const patternColumn = Math.floor(currentLocation / currentLine.length);
  const currentLocationInPattern =
    currentLocation - patternColumn * currentLine.length;
  return currentLocationInPattern;
}

function isTree(currentLine, currentLocationInPattern) {
  return currentLine[currentLocationInPattern] === "#";
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
