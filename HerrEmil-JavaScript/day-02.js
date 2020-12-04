const parseListItem = (line) => line.split(/-|: | /);
const parseData = (data) => data.split(/\r?\n/).map(parseListItem);

const isCharCountWithinRange = ([min, max, char, string]) => {
  const num = string.split(char).length - 1;
  return min <= num && num <= max;
};

const isCharAtExclusivelyOnePosition = ([first, second, char, string]) => {
  const firstMatch = string.charAt(first - 1) === char;
  const SecondMatch = string.charAt(second - 1) === char;

  return firstMatch !== SecondMatch;
};

module.exports.isCharAtExclusivelyOnePosition = isCharAtExclusivelyOnePosition;
module.exports.isCharCountWithinRange = isCharCountWithinRange;
module.exports.solve1 = (data) => data.filter(isCharCountWithinRange).length;
module.exports.solve2 = (data) =>
  data.filter(isCharAtExclusivelyOnePosition).length;
module.exports.parseDay02ListItem = parseListItem;
module.exports.parseData = parseData;
