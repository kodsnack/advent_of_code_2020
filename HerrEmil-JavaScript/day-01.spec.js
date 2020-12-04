const { strictEqual } = require("assert");
const {
  findPairWithSum2020,
  findTripleWithSum2020,
  parseData,
} = require("./day-01");

describe("Day 01 tests", () => {
  const testInput = `1721
979
366
299
675
1456`;
  const data = parseData(testInput);

  it("should find a pair and multiply them", () => {
    strictEqual(findPairWithSum2020(data), 514579)
  });
  it("should find a triple and multiply them", () => {
    strictEqual(findTripleWithSum2020(data), 241861950)
  });
});
