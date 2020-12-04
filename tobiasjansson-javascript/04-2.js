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
   // field(passport, 'cid', line); // (Country ID)
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
        let v = fields[1];
        switch (f) {
            case 'byr':
                if (parseInt(v) > 1919 && parseInt(v) < 2003) passport[f] = v;
                break;
            case 'iyr':
                if (parseInt(v) > 2009 && parseInt(v) < 2021) passport[f] = v;
                break;
            case 'eyr':
                if (parseInt(v) > 2019 && parseInt(v) < 2031) passport[f] = v;
                break;
            case 'hgt':
                if (v.endsWith('cm') && (parseInt(v) > 149 && parseInt(v) < 194)) passport[f] = v;
                if (v.endsWith('in') && (parseInt(v) > 58 && parseInt(v) < 77)) passport[f] = v;
                break;
            case 'hcl':
                let color = v.match(/#[a-f0-9]{6}/);
                if(color != null) passport[f] = v;
                break;
            case 'ecl':
                if(['amb','blu','brn','gry','grn','hzl','oth'].includes(v)) passport[f] = v
                break;
            case 'pid':
                let pid = v.match(/^\d{9}$/);
                if(pid != null) passport[f] = v;
                break;
            // case 'cid':
            //     passport[f] = v
            //     break;
            default:
                console.log('okänt fält ' + f);
                break;
        }
    }
}

function validate(passport) {
    let numkeys = Object.keys(passport).length;
    //let cid = 'cid' in passport;
    //let valid = (numkeys == 8 || (numkeys == 7 && cid == false));
    let valid = numkeys == 7;
    return valid;
}