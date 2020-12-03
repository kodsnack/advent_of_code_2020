const { strictEqual } = require("assert");
const { isCharAtExclusivelyOnePosition, isCharCountWithinRange, parseDay02ListItem } = require("./day-02");

describe("Day 02, Puzzle 1 tests", () => {
  it('should find 1 to 3 "a" in "abcde"', () => {
    const data = parseDay02ListItem("1-3 a: abcde");
    const result = isCharCountWithinRange(data);
    strictEqual(result, true);
  });
  it('should not find 1 to 3 "b" in "cdefg"', () => {
    const data = parseDay02ListItem("1-3 b: cdefg");
    const result = isCharCountWithinRange(data);
    strictEqual(result, false);
  });
  it('should find 2 to 9 "c" in "ccccccccc"', () => {
    const data = parseDay02ListItem("2-9 c: ccccccccc");
    const result = isCharCountWithinRange(data);
    strictEqual(result, true);
  });
});

describe("Day 02, Puzzle 2 tests", () => {
  it('should find "a" in one of positions in "abcde"', () => {
    const data = parseDay02ListItem("1-3 a: abcde");
    const result = isCharAtExclusivelyOnePosition(data);
    strictEqual(result, true);
  });
  it('should not find "b" in either of positions in "cdefg"', () => {
    const data = parseDay02ListItem("1-3 b: cdefg");
    const result = isCharAtExclusivelyOnePosition(data);
    strictEqual(result, false);
  });
  it('should not find "c" in only one of positions in "ccccccccc"', () => {
    const data = parseDay02ListItem("2-9 c: ccccccccc");
    const result = isCharAtExclusivelyOnePosition(data);
    strictEqual(result, false);
  });
});
