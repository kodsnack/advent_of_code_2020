const fs = require('fs');

async function read() {
  return await (await fs.promises.readFile('./inputs.txt', 'utf-8')).split('\n');
}

async function tobogganloc(inc, d = 1) {
  const values = await read();
  values.shift();
  let i = inc;
  let treec = 0;
  values.forEach((row) => {
    const rowI = i % row.length;
    if (row[rowI] == '#') {
      treec += 1;
    }
    i += inc;
  });
  console.log(treec);
}
tobogganloc(1); //86
tobogganloc(3); //232
tobogganloc(5); //90
tobogganloc(7); //71