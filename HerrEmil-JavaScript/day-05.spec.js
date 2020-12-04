const { strictEqual } = require("assert");
const { solve1, solve2, parseData } = require("./day-05");

describe("Day 05 tests", () => {
  const testInput = ``;
  const data = parseData(testInput);

  it("should", () => {
    const result = solve1(data);
    strictEqual(result, true);
  });

  it("should", () => {
    const result = solve2(data);
    strictEqual(result, true);
  });
});
