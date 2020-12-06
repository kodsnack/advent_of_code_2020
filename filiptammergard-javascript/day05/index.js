const fs = require("fs");

const input = fs.readFileSync("day05/input.txt").toString().split("\n");

function computePartOne() {
  const ids = getIds(input);
  const max = Math.max(...ids);
  return max;
}

function computePartTwo() {
  const ids = getIds(input);
  const myId = getMissingId(ids);
  return myId;
}

function getIds(codes) {
  const ids = codes.reduce((ids, inputRow) => {
    const row = decodeRow(inputRow);
    const column = decodeColumn(inputRow);
    const id = row[0] * 8 + column[0];
    return [...ids, id];
  }, []);
  return ids;
}

function decodeRow(code) {
  const row = decode(code.substr(0, 7), "F", "B", [0, 127]);
  return row;
}

function decodeColumn(code) {
  const column = decode(code.substr(7), "L", "R", [0, 7]);
  return column;
}

function decode(code, lowerLetter, upperLetter, range) {
  const column = code.split("").reduce((range, curr) => {
    const diff = (range[1] - range[0] + 1) / 2;
    if (curr === lowerLetter) {
      return [range[0], range[1] - diff];
    } else if (curr === upperLetter) {
      return [range[0] + diff, range[1]];
    }
  }, range);
  return column;
}

function getMissingId(ids) {
  const id = ids
    .sort((a, b) => a - b)
    .find((id, index, array) => {
      if (array[index + 1] !== id + 1) {
        return true;
      }
    });
  return id + 1;
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
