const fs = require('fs');

function getUniqueVotes(gv) {
  const uv = new Set();
  gv.forEach((pv) => {
    const v = pv.split("");
    v.forEach((vote) => {
      uv.add(vote);
    });
  });
  return [...uv];
}

function valid1() {
  const values = fs.readFileSync('./inputs.txt');
  const inputs = values.toString().split("\n\n");
  return inputs.reduce((sum, gi) => {
    const gpVotes = gi.split("\n");
    const uVotes = getUniqueVotes(gpVotes);
    return sum + uVotes.length;
  }, 0);
}

function valid2() {
  const values = fs.readFileSync('./inputs.txt');
  const inputs = values.toString().split("\n\n");
  return inputs.reduce((sum, gi) => {
    const gpVotes = gi.split("\n");
    const uVotes = getUniqueVotes(gpVotes);
    const cgVotes = uVotes.filter((vote) =>
      gpVotes.every((pVotes) => pVotes.includes(vote))
    );
    return sum + cgVotes.length;
  }, 0);
}

console.log(valid1());
console.log(valid2())