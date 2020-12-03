const { strictEqual } = require("assert");
const { solve1, solve2, parseDay04Data } = require("./day-04");

describe("Day 04 tests", () => {
  const testInput = ``;
  const data = parseDay04Data(testInput);

  it("should", () => {
    const result = solve1(data);
    strictEqual(result, true);
  });

  it("should", () => {
    const result = solve2(data);
    strictEqual(result, true);
  });
});
