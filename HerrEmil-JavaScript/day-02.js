const parseListItem = (line) => line.split(/-|: | /);

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
module.exports.parseDay02ListItem = parseListItem;
