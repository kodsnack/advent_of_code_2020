const fs = require('fs');

async function read() {
  return await (await fs.promises.readFile('./inputs.txt', 'utf-8')).split('\n');
}

async function tobogganloc(inc, d = 1) {
  const values = await read();
  //values.shift();
  let increment = inc;
  let treec = 0;
  for (let i = d; i < values.length; i += d) {
    const row = values[i];
    const rowI = increment % row.length;
    if (row[rowI] == '#') {
      treec += 1;
    }
    increment += inc;
  }
  return treec;
}

async function ans() {
  const answer = Promise.all([tobogganloc(1), tobogganloc(3), tobogganloc(5), tobogganloc(7), tobogganloc(1, 2)])
    .then((answer) => {
      console.log(answer);
      return answer.reduce((product, value) => product * value, 1);
    })
    .then(console.log);
}

ans();