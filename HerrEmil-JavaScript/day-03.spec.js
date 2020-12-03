const { strictEqual } = require("assert");
const { solve1, solve2, parseDay03Data } = require("./day-03");

describe("Day 03 tests", () => {
  const testInput = `..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#`;
  const data = parseDay03Data(testInput);

  it("should find 7 trees following a slope", () => {
    const result = solve1(data, 3, 1);
    strictEqual(result, 7);
  });

  it("should find a series of trees and multiply them", () => {
    const result = solve2(data, [
      [1, 1],
      [3, 1],
      [5, 1],
      [7, 1],
      [1, 2],
    ]);
    strictEqual(result, 336);
  });
});
