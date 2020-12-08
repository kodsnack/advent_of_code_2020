const { strictEqual } = require("assert");
const { getSeatID } = require("./day-05");

describe("Day 05 tests", () => {
  it("should find seat IDs", () => {
    strictEqual(getSeatID("FBFBBFFRLR"), 357);
    strictEqual(getSeatID("BFFFBBFRRR"), 567);
    strictEqual(getSeatID("FFFBBBFRRR"), 119);
    strictEqual(getSeatID("BBFFBBFRLL"), 820);
  });
});
