const parseData = (data) => data.split(/\r?\n/).map((line) => line);

// B 1
// F 0
// R 1
// L 0
const parseSeat = (seat = "") =>
  seat
    .split("")
    .map((char) =>
      char === "B" ? 1 : char === "F" ? 0 : char === "R" ? 1 : 0
    );

const getSeatID = (data) => {
  const parsedSeat = parseSeat(data);

  let rows = Array.from(Array(128), (_, index) => index);
  let columns = Array.from(Array(8), (_, index) => index);

  for (let i = 0; i < 7; i += 1) {
    const half = Math.ceil(rows.length / 2);

    rows = parsedSeat[i] ? rows.splice(-half) : rows.splice(0, half);
  }

  for (let j = 0; j < 3; j += 1) {
    const half = Math.ceil(columns.length / 2);

    columns = parsedSeat[7 + j]
      ? columns.splice(-half)
      : columns.splice(0, half);
  }

  return rows[0] * 8 + columns[0];
};

const solve1 = (data) =>
  data
    .map(getSeatID)
    .sort((a, b) => a - b)
    .pop();

const solve2 = (data) => {
  const sorted = data.map(getSeatID).sort((a, b) => a - b);

  for (let i = 0; i < sorted.length; i += 1) {
    if (i + sorted[0] !== sorted[i]) {
      return sorted[i] - 1;
    }
  }
};

module.exports.getSeatID = getSeatID;
module.exports.solve1 = solve1;
module.exports.solve2 = solve2;
module.exports.parseData = parseData;
