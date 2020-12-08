const parseData = (data) => data.split(/\r?\n\r?\n/);

const uniqueAnswers = (group) => {
  const answers = group.split(/\r?\n/).join("");

  const uniqueAnswers = [];

  for (answer of answers) {
    if (!uniqueAnswers.includes(answer)) {
      uniqueAnswers.push(answer);
    }
  }

  return uniqueAnswers;
};

const commonAnswers = (group) => {
  const answers = group.split(/\r?\n/);

  const commonAnswers = [];
  const uniques = uniqueAnswers(group);

  for (unique of uniques) {
    if (answers.every((answer) => answer.includes(unique))) {
      commonAnswers.push(unique);
    }
  }

  return commonAnswers.length;
};

const solve1 = (data) => {
  const counts = data.map(uniqueAnswers).map((a) => a.length);

  return counts.reduce((sum, count) => sum + count, 0);
};

const solve2 = (data) => {
  const counts = data.map(commonAnswers);

  return counts.reduce((sum, count) => sum + count, 0);
};

module.exports.solve1 = solve1;
module.exports.solve2 = solve2;
module.exports.parseData = parseData;
