const fs = require("fs");

const input = fs.readFileSync("day08/input.txt").toString().split("\n");

function computePartOne() {
  const instructions = parseInstructions(input);

  let accumulator = 0;
  let runInstructios = [];
  let i = 0;

  while (!runInstructios.includes(i)) {
    runInstructios.push(i);
    if (instructions[i].operation === "acc") {
      accumulator += instructions[i].argument;
      i += 1;
    } else if (instructions[i].operation === "jmp") {
      i += instructions[i].argument;
    } else if (instructions[i].operation === "nop") {
      i += 1;
    }
  }

  return accumulator;
}

function computePartTwo() {
  const instructions = parseInstructions(input);

  let accumulator = 0;
  let runInstructios = [];
  let i = 0;
  let j = 0;
  let finished = false;
  instructionsCopy = JSON.parse(JSON.stringify(instructions));
  const changableInstructionIndexes = instructions.reduce(
    (indexes, instruction, index) => {
      return instruction.operation === "jmp" || instruction.operation === "nop"
        ? [...indexes, index]
        : indexes;
    },
    []
  );

  while (!finished) {
    instructionsCopy = JSON.parse(JSON.stringify(instructions));
    accumulator = 0;
    i = 0;
    runInstructios = [];

    if (instructionsCopy[changableInstructionIndexes[j]].operation === "jmp") {
      instructionsCopy[changableInstructionIndexes[j]].operation = "nop";
    } else if (
      instructionsCopy[changableInstructionIndexes[j]].operation === "nop"
    ) {
      instructionsCopy[changableInstructionIndexes[j]].operation = "jmp";
    }
    j++;

    while (!runInstructios.includes(i)) {
      if (i >= instructions.length - 1) {
        finished = true;
        break;
      }
      runInstructios.push(i);
      if (instructionsCopy[i].operation === "acc") {
        accumulator += instructionsCopy[i].argument;
        i += 1;
      } else if (instructionsCopy[i].operation === "jmp") {
        i += instructionsCopy[i].argument;
      } else if (instructionsCopy[i].operation === "nop") {
        i += 1;
      }
    }
  }
  return accumulator;
}

function parseInstructions(input) {
  const instructions = input.map((row) => {
    return {
      operation: row.split(" ")[0],
      argument: parseInt(row.split(" ")[1]),
    };
  });
  return instructions;
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
