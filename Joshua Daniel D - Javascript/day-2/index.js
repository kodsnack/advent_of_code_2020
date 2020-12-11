const fs = require('fs');

async function read() {
  return await fs.promises.readFile('./inputs.txt', 'utf-8');
}

async function passpass() {
  var vpass = [];
  const passwords = await read((key) => +key);
  const inputs = passwords.toString().split("\n");
  for (var i = 0; i < inputs.length; i++) {
    const line = inputs[i];
    const n = line.slice(0, 5);
    const num = n.split(" ");
    const nrule = num[0].split("-");
    const minrule = nrule[0];
    const maxrule = nrule[1];
    const text = line.slice(5, line.length);
    const tpw = text.split(" ");
    const pw = tpw[tpw.length - 1].toString();
    const lt = line.slice(0, 7).split(" ");
    if (lt[2] === '') {
      lt.pop(lt[2]);
    }
    const lc = lt[1].split(":");
    if (lc[1] === '') {
      lc.pop(lc[1]);
    }
    const lrule = lc[0].toString();

    var letc = letterCount(pw, lrule, (value) => +letc);
    if (letc >= minrule && letc < maxrule) {
      vpass.push(pw);
      console.log(vpass.length);
    } else {
      console.log("It's NOT VALID!")
    }

    function letterCount(str, letter) {
      var letter_Count = 0;
      for (var position = 0; position < str.length; position++) {
        if (str.charAt(position) == letter) {
          letter_Count += 1;
        }
      }
      return letter_Count;
    }

  }
}

async function passpw() {
  var vpass = [];
  const passwords = await read((key) => +key);
  const inputs = passwords.toString().split("\n");
  for (var i = 0; i < inputs.length; i++) {
    const line = inputs[i];
    const n = line.slice(0, 5);
    const num = n.split(" ");
    const nrule = num[0].split("-");
    const minrule = parseFloat(nrule[0]);
    const maxrule = parseFloat(nrule[1]);
    const text = line.slice(5, line.length);
    const tpw = text.split(" ");
    const pw = tpw[tpw.length - 1].toString();
    const lt = line.slice(0, 7).split(" ");
    if (lt[2] === '') {
      lt.pop(lt[2]);
    }
    const lc = lt[1].split(":");
    if (lc[1] === '') {
      lc.pop(lc[1]);
    }
    const lrule = lc[0].toString();
    letterCount(pw, minrule, maxrule);

    function letterCount(str, num1, num2) {
      var n1 = num1 - 1;
      var n2 = num2 - 1;
      console.log(str.charAt(n1), str.charAt(n2));
      if (str.charAt(n1) == lrule && str.charAt(n1) != str.charAt(n2)) {
        vpass.push(pw);
        console.log(vpass.length);
      } else {
        console.log("NOT VALID!");
      }

      if (str.charAt(n2) == lrule && str.charAt(n2) != str.charAt(n1)) {
        vpass.push(pw);
        console.log(vpass.length);
      } else {
        console.log("NOT VALID!");
      }
    }
  }
}
passpw();
//passpass();