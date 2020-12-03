const parseDay03Data = (data) =>
  data
    .split(/\r?\n/)
    .map((line) => line.split("").map((char) => (char === "." ? 0 : 1)));

const solve1 = (data, right = 3, down = 1) =>
  data
    .map((line, lineIndex) => {
      if (lineIndex % down) {
        return 0;
      }
      const columnIndex = (lineIndex / down) * right;
      return line[columnIndex % line.length];
    })
    .map((number) => parseInt(number, 10))
    .filter((number) => number).length;

const solve2 = (data, slopes) =>
  slopes
    .map(([right, down]) => solve1(data, right, down))
    .reduce((cumulative, current) => cumulative * current, 1);

module.exports.solve1 = solve1;
module.exports.solve2 = solve2;
module.exports.parseDay03Data = parseDay03Data;
