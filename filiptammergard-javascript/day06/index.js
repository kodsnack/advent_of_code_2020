const fs = require("fs");

const input = fs.readFileSync("day06/input.txt").toString().split("\n\n");

function computePartOne() {
  return input.reduce((sum, groupInput) => {
    const groupVotes = groupInput.split("\n");
    const uniqueVotes = getUniuqeVotes(groupVotes);
    return sum + uniqueVotes.length;
  }, 0);
}

function computePartTwo() {
  return input.reduce((sum, groupInput) => {
    const groupVotes = groupInput.split("\n");
    const uniqueVotes = getUniuqeVotes(groupVotes);
    const commonGroupVotes = uniqueVotes.filter((vote) =>
      groupVotes.every((personVotes) => personVotes.includes(vote))
    );
    return sum + commonGroupVotes.length;
  }, 0);
}

function getUniuqeVotes(groupVotes) {
  const uniqueVotes = new Set();
  groupVotes.forEach((personVotes) => {
    const votes = personVotes.split("");
    votes.forEach((vote) => {
      uniqueVotes.add(vote);
    });
  });
  return [...uniqueVotes];
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
