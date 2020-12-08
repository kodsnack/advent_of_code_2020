const { strictEqual } = require("assert");
const { solve1, solve2, parseData } = require("./day-06");

describe("Day 06, Puzzle 1 tests", () => {
  const testInput = `abc

a
b
c

ab
ac

a
a
a
a

b`;
  const data = parseData(testInput);

  it("should count unique answers in each group and sum counts", () => {
    const result = solve1(data);
    strictEqual(result, 11);
  });

  it("should count common answers in each group and sum counts", () => {
    const result = solve2(data);
    strictEqual(result, 6);
  });
});
