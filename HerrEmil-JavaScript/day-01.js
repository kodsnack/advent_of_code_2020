const findPairWithSum2020 = (input) => {
  for (i = 0; i < input.length; i += 1) {
    for (j = i; j < input.length; j += 1) {
      if (input[i] + input[j] === 2020) {
        return input[i] * input[j];
      }
    }
  }
};

const findTripleWithSum2020 = (input) => {
  for (i = 0; i < input.length; i += 1) {
    for (j = i; j < input.length; j += 1) {
      for (k = j; k < input.length; k += 1) {
        if (input[i] + input[j] + input[k] === 2020) {
          return input[i] * input[j] * input[k];
        }
      }
    }
  }
};

const parseData = (string) =>
  string.split(/\r?\n/).map((number) => parseInt(number, 10));

module.exports.findPairWithSum2020 = findPairWithSum2020;
module.exports.findTripleWithSum2020 = findTripleWithSum2020;
module.exports.solve1 = findPairWithSum2020;
module.exports.solve2 = findTripleWithSum2020;
module.exports.parseData = parseData;
