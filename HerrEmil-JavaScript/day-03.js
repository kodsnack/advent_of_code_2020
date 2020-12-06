const parseData = (data) =>
  data
    .split(/\r?\n/)
    .map((line) => line.split("").map((char) => (char === "." ? 0 : 1)));

const treesOnSlope = (data, right = 3, down = 1) =>
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

const productOfTreesOnSlopes = (data, slopes) =>
  slopes
    .map(([right, down]) => treesOnSlope(data, right, down))
    .reduce((cumulative, current) => cumulative * current, 1);

module.exports.treesOnSlope = treesOnSlope;
module.exports.productOfTreesOnSlopes = productOfTreesOnSlopes;
module.exports.solve1 = (data) => treesOnSlope(data, 3, 1);
module.exports.solve2 = (data) =>
  productOfTreesOnSlopes(data, [
    [1, 1],
    [3, 1],
    [5, 1],
    [7, 1],
    [1, 2],
  ]);
module.exports.parseData = parseData;
