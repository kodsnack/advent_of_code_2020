const fs = require('fs');

async function read() {
  return await fs.promises.readFile('./inputs.txt', 'utf-8');
}

async function addmult() {
  const values = await read((value) => +value);
  const inputs = values.split('\n');
  for (var i = 0; i < inputs.length; i++) {
    var num = parseFloat(inputs[i]);
    for (var l = 0; l < inputs.length; l++) {
      var num2 = parseFloat(inputs[i - l]);
      var ans = num + num2;
      if (ans == 2020) {
        console.log("Pair:" + num * num2);
      }
      for (var k = 0; k < inputs.length; k++) {
        var num3 = parseFloat(inputs[l - k]);
        var ans = num + num2 + num3;
        if (ans == 2020) {
          console.log("Triplet:" + num * num2 * num3);
        }
      }
    }
  }
}

addmult();