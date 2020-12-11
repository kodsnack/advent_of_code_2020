const fs = require('fs');

function fseat() {
  const inputs = fs.readFileSync('./inputs.txt', 'utf-8').toString().split("\n");

  function p1() {
    const ids = gI(inputs);
    const max = Math.max(...ids);
    return max;
  }

  function p2() {
    const ids = gI(inputs);
    const myId = gmI(ids);
    return myId;
  }

  function gI(codes) {
    const ids = codes.reduce((ids, inputRow) => {
      const row = dr(inputRow);
      const column = dc(inputRow);
      const id = row[0] * 8 + column[0];
      return [...ids, id];
    }, []);
    return ids;
  }

  function dr(code) {
    const row = d(code.substr(0, 7), "F", "B", [0, 127]);
    return row;
  }

  function dc(code) {
    const column = d(code.substr(7), "L", "R", [0, 7]);
    return column;
  }

  function d(code, lowerLetter, upperLetter, range) {
    const column = code.split("").reduce((range, curr) => {
      const diff = (range[1] - range[0] + 1) / 2;
      if (curr === lowerLetter) {
        return [range[0], range[1] - diff];
      } else if (curr === upperLetter) {
        return [range[0] + diff, range[1]];
      }
    }, range);
    return column;
  }

  function gmI(ids) {
    const id = ids
      .sort((a, b) => a - b)
      .find((id, index, array) => {
        if (array[index + 1] !== id + 1) {
          return true;
        }
      });
    return id + 1;
  }

  console.log(p1());
  console.log(p2());
}

fseat();