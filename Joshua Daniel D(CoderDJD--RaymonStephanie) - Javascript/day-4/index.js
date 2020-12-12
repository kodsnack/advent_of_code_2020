const fs = require('fs');

async function read() {
  return await fs.promises.readFile('./inputs.txt', 'utf-8');
}

async function valid() {
  var n = 0;
  const values = await read();
  const value = values.toString().split('\r\n');
  const input = value.toString().split('\n');
  const inputs = input.toString().split('\r').toString().split(",,");
  for (let i = 0; i < inputs.length; i++) {
    var byrt = false;
    var iyrt = false;
    var eyrt = false;
    var hgtt = false;
    var hclt = false;
    var eclt = false;
    var pidt = false;
    // cid (Country ID)
    // byr (Birth Year)
    const byr = inputs[i].search("byr:");
    // iyr (Issue Year)
    const iyr = inputs[i].search("iyr:");
    // eyr (Expiration Year)
    const eyr = inputs[i].search("eyr:");
    // hgt (Height)
    const hgt = inputs[i].search("hgt:");
    // hcl (Hair Color)
    const hcl = inputs[i].search("hcl:");
    // ecl (Eye Color)
    const ecl = inputs[i].search("ecl:");
    // pid (Passport ID)
    const pid = inputs[i].search("pid:");
    if (byr != -1 && iyr != -1 && eyr != -1 && hgt != -1 && hcl != -1 && ecl != -1 && pid != -1) {
      // n++;
      // console.log(n);
    }
    const line = inputs[i].toString();
    const by1 = (byr + 4).toString();
    const by2 = (byr + 5).toString();
    const by3 = (byr + 6).toString();
    const by4 = (byr + 7).toString();
    const bys = line.charAt(by1) + line.charAt(by2) + line.charAt(by3) + line.charAt(by4).trim();
    const by = parseFloat(bys);
    const iy1 = (iyr + 4).toString();
    const iy2 = (iyr + 5).toString();
    const iy3 = (iyr + 6).toString();
    const iy4 = (iyr + 7).toString();
    const iys = line.charAt(iy1) + line.charAt(iy2) + line.charAt(iy3) + line.charAt(iy4).trim();
    const iy = parseFloat(iys);
    const ey1 = (eyr + 4).toString();
    const ey2 = (eyr + 5).toString();
    const ey3 = (eyr + 6).toString();
    const ey4 = (eyr + 7).toString();
    const eys = line.charAt(ey1) + line.charAt(ey2) + line.charAt(ey3) + line.charAt(ey4).trim();
    const ey = parseFloat(eys);
    const h1 = hgt + 4;
    const h2 = hgt + 5;
    const h3 = hgt + 6;
    const h4 = hgt + 7;
    const h5 = hgt + 8;
    const hgs = line.charAt(h1) + line.charAt(h2) + line.charAt(h3) + line.charAt(h4) + line.charAt(h5).trim();
    const hc1 = hcl + 4;
    const hc2 = hcl + 5;
    const hc3 = hcl + 6;
    const hc4 = hcl + 7;
    const hc5 = hcl + 8;
    const hc6 = hcl + 9;
    const hc7 = hcl + 10;
    const hc8 = hcl + 11;
    const hc9 = hcl + 12;
    const hc10 = hcl + 13;
    const hc11 = hcl + 14;
    const hc12 = hcl + 15;
    const hc13 = hcl + 16;
    const hc14 = hcl + 17;
    const hc15 = hcl + 18;
    const hc = line.charAt(hc1) + line.charAt(hc2) + line.charAt(hc3) + line.charAt(hc4) + line.charAt(hc5) + line.charAt(hc6) + line.charAt(hc7) + line.charAt(hc8) + line.charAt(hc9) + line.charAt(hc10) + line.charAt(hc11) + line.charAt(hc12) + line.charAt(hc13) + line.charAt(hc14) + line.charAt(hc15).trim();
    console.log(hc);

    function isAlphaNumeric(str) {
      var code, i, len;
      for (i = 0, len = str.length; i < len; i++) {
        code = str.charCodeAt(i);
        if (!(code > 47 && code < 58) && // numeric (0-9)
          !(code > 64 && code < 91) && // upper alpha (A-Z)
          !(code > 96 && code < 123)) { // lower alpha (a-z)
          return false;
        };
      }
      return true;
    }
    const e1 = ecl + 4;
    const e2 = ecl + 5;
    const e3 = ecl + 6;
    const en = line.charAt(e1) + line.charAt(e2) + line.charAt(e3).trim();
    const p1 = pid + 4;
    const p2 = pid + 5;
    const p3 = pid + 6;
    const p4 = pid + 7;
    const p5 = pid + 8;
    const p6 = pid + 9;
    const p7 = pid + 10;
    const p8 = pid + 11;
    const p9 = pid + 12;
    const pns = line.charAt(p1) + line.charAt(p2) + line.charAt(p3) + line.charAt(p4) + line.charAt(p5) + line.charAt(p6) + line.charAt(p7) + line.charAt(p8) + line.charAt(p9);
    const pn = pns.slice(0, pns.length).trim();
    if (by >= 1920 && by <= 2002) {
      byrt = true;
    }
    if (iy >= 2010 && iy <= 2020) {
      iyrt = true;
    }
    if (ey >= 2020 && ey <= 2030) {
      eyrt = true;
    }
    if (hgs.search("in") != -1) {
      const hnis = hgs.trim().split("in").toString().split(",");
      const hni = parseFloat(hnis[0]);
      if (hni <= 76 && hni >= 59) {
        hgtt = true;
      }
    }
    if (hgs.search("cm") != -1) {
      const hcmis = hgs.trim().split("in").toString().split(",");
      const hcmi = parseFloat(hcmis[0]);
      if (hcmi <= 193 && hcmi >= 150) {
        hgtt = true;
      }
    }
    if ( /*hc.length == 7 && */ hc.startsWith("#", 0)) {
      if (isAlphaNumeric(hc.slice(1, hc.length)) == true) {
        console.log(hc.slice(1, hc.length));
        hclt = true;
      }
    }
    if (en.includes("amb") || en.includes("blu") || en.includes("brn") || en.includes("gry") || en.includes("grn") || en.includes("hzl") || en.includes("oth")) {
      eclt = true;
    }
    if (pn.length == 9) {
      pidt = true;
    }
    if (
      byrt == true &&
      iyrt == true &&
      eyrt == true &&
      hgtt == true &&
      hclt == true &&
      eclt == true &&
      pidt == true
    ) {
      n++;
      console.log(n);
    }
  }
}

valid();