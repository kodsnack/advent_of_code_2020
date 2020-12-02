const fs = require("fs");

const input = fs.readFileSync("day02/input.txt").toString().split("\n");

function computePartOne() {
  const answer = input.reduce((correctAnswers, currentLine) => {
    const password = decodePassword(currentLine);
    const decoding = decodePasswordPolicy(currentLine);
    const isValid = checkPasswordValidityPart1(password, decoding);
    return isValid ? correctAnswers + 1 : correctAnswers;
  }, 0);

  return answer;
}

function decodePasswordPolicy(line) {
  const part1 = line.split("-")[0];
  const part2 = line.split("-")[1].split(" ")[0];
  const letter = line.split(" ")[1][0];
  const decoding = {
    part1,
    part2,
    letter,
  };
  return decoding;
}

function decodePassword(line) {
  const password = line.split(" ")[2];
  return password;
}

function checkPasswordValidityPart1(password, decoding) {
  const isMinLimitOK = password.split(decoding.letter).length > decoding.part1;
  const isMaxLimitOK =
    password.split(decoding.letter).length - 1 <= decoding.part2;
  return isMinLimitOK && isMaxLimitOK;
}

function computePartTwo() {
  const answer = input.reduce((correctAnswers, currentLine) => {
    const password = decodePassword(currentLine);
    const decoding = decodePasswordPolicy(currentLine);
    const isValid = checkPasswordValidityPart2(password, decoding);
    return isValid ? correctAnswers + 1 : correctAnswers;
  }, 0);

  return answer;
}

function checkPasswordValidityPart2(password, decoding) {
  const positionOneLetter = password[+decoding.part1 - 1];
  const positionTwoLetter = password[+decoding.part2 - 1];
  const isPositionOneLetterCorrect = positionOneLetter === decoding.letter;
  const isPositionTw0LetterCorrect = positionTwoLetter === decoding.letter;
  const isOnlyOneCorrect =
    [isPositionOneLetterCorrect, isPositionTw0LetterCorrect].filter(
      (item) => item
    ).length === 1;
  return isOnlyOneCorrect;
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
