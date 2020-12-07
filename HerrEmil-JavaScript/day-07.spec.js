const { strictEqual } = require("assert");
const { solve1, solve2, parseData } = require("./day-07");

describe("Day 07, Puzzle 1 tests", () => {
  const testInput = ``;
  const data = parseData(testInput);

  it("should ", () => {
    const result = solve1(data);
    strictEqual(result, 11);
  });

  it("should ", () => {
    const result = solve2(data);
    strictEqual(result, 6);
  });
});
