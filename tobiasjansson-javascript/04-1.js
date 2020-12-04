const fs = require('fs');
const args = process.argv.slice(2);

const data = fs.readFileSync(args[0], 'utf8');

let lines = data.split('\n');

let passports = [];
let passport = {};
lines.forEach(line => {
    if (line.length <= 1) {
        passports.push(passport);
        passport = {};
    }

    field(passport, 'byr', line); // (Birth Year)
    field(passport, 'iyr', line); // (Issue Year)
    field(passport, 'eyr', line); // (Expiration Year)
    field(passport, 'hgt', line); // (Height)
    field(passport, 'hcl', line); // (Hair Color)
    field(passport, 'ecl', line); // (Eye Color)
    field(passport, 'pid', line); // (Passport ID)
    field(passport, 'cid', line); // (Country ID)
});
passports.push(passport);

let numvalid = passports.filter(p => validate(p)).length;
console.log('Godkända: ' + numvalid);

function field(passport, f, line) {
    if (f in passport) {
        return;
    }
    let regex = new RegExp(f + ':([#a-z0-9]*)');
    let fields = line.match(regex);
    if (fields != null) {
        passport[f] = fields[1];
    }
}

function validate(passport) {
    let numkeys = Object.keys(passport).length;
    let cid = 'cid' in passport;

    return (numkeys == 8 || (numkeys == 7 && cid == false));
}