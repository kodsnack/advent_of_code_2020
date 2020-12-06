const fs = require("fs");

const input = fs.readFileSync("day04/input.txt").toString().split("\n\n");

function computePartOne() {
  const passports = decodePassports(input);

  const validPassports = passports.filter((passport) =>
    requiredFieldsExsists(passport)
  );

  return validPassports.length;
}

function computePartTwo() {
  const passports = decodePassports(input);

  const validPassports = passports.filter(
    (passport) =>
      requiredFieldsExsists(passport) &&
      isValidByr(passport["byr"]) &&
      isValidIyr(passport["iyr"]) &&
      isValidEyr(passport["eyr"]) &&
      isValidHgt(passport["hgt"]) &&
      isValidHcl(passport["hcl"]) &&
      isValidHcl(passport["hcl"]) &&
      isValidEcl(passport["ecl"]) &&
      isValidPid(passport["pid"])
  );

  return validPassports.length;
}

function decodePassports(input) {
  return input
    .map((passport) => passport.split(/ |\n/))
    .reduce((acc, curr) => {
      const decoded = curr.reduce((acc, curr) => {
        const [key, value] = curr.split(":");
        return { ...acc, [key]: value };
      }, {});
      return [...acc, decoded];
    }, []);
}

function requiredFieldsExsists(passport) {
  const requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
  return requiredFields.every((field) => field in passport);
}

function isValidByr(byr) {
  return checkLengthMinMax(byr, 4, 1920, 2002);
}

function isValidIyr(iyr) {
  return checkLengthMinMax(iyr, 4, 2010, 2020);
}

function isValidEyr(eyr) {
  return checkLengthMinMax(eyr, 4, 2020, 2030);
}

function checkLengthMinMax(value, length, min, max) {
  return value.length === length && value >= min && value <= max;
}

function isValidHgt(hgt) {
  if (hgt.endsWith("cm")) {
    const height = hgt.split("cm")[0];
    if (height >= 150 && height <= 193) return true;
    else return false;
  } else if (hgt.endsWith("in")) {
    const height = hgt.split("in")[0];
    if (height >= 59 && height <= 76) return true;
    else return false;
  } else return false;
}

function isValidHcl(hcl) {
  hclRegex = RegExp(/#[0-9a-f]{6}/);
  return hclRegex.test(hcl);
}

function isValidEcl(ecl) {
  return ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].includes(ecl);
}

function isValidPid(pid) {
  return !isNaN(pid) && pid.length === 9;
}

const partOneAnswer = computePartOne();
console.log(`Part 1: ${partOneAnswer}`);

const partTwoAnswer = computePartTwo();
console.log(`Part 2: ${partTwoAnswer}`);
