const {
  group
} = require('console');
const fs = require('fs');

async function read() {
  return await fs.promises.readFile('./inputs.txt', 'utf-8');
}

async function valid() {
  var n = 0;
  const values = await read();
  const value = values.toString().split('\r\n\n');
  const input = value.toString().split('\n');
  const inputs = input.toString().split('\r').toString()
  const groups = inputs.split(",,");
  console.log(groups);
  for (let i = 0; i < groups.length; i++) {
    const answers = groups[0].split(',');
    console.log(answers);
  }
}

valid();