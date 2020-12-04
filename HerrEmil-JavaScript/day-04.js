const parseData = (data) =>
  data.split(/\r?\n\r?\n/).map((passport) =>
    Object.assign(
      ...passport
        .split(/\s+/)
        .map((field) => field.split(":"))
        .map(([key, value]) => ({ [key]: value }))
    )
  );

const requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

const solve1 = (data) =>
  data.filter((passport) =>
    requiredFields.every((field) => passport.hasOwnProperty(field))
  ).length;

const validByr = (byr) => {
  if (!byr) return false;
  const byrNum = parseInt(byr, 10);
  return 1920 <= byrNum && byrNum <= 2002;
};

const validIyr = (byr) => {
  if (!byr) return false;
  const iyrNum = parseInt(byr, 10);
  return 2010 <= iyrNum && iyrNum <= 2020;
};

const validEyr = (byr) => {
  if (!byr) return false;
  const eyrNum = parseInt(byr, 10);
  return 2020 <= eyrNum && eyrNum <= 2030;
};

const validHgt = (hgt) => {
  if (!hgt) return false;
  hgtNum = parseInt(hgt.split(/cm|in/), 10);
  if (hgt.endsWith("cm")) {
    return 150 <= hgtNum && hgtNum <= 193;
  }
  if (hgt.endsWith("in")) {
    return 59 <= hgtNum && hgtNum <= 76;
  }
  return false;
};

const validHcl = (hcl) => /^#[0-9a-f]{6}$/.test(hcl);

const validEcl = (ecl) => {
  if (!ecl) return false;
  return ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].includes(ecl);
};

const validPid = (pid) => {
  if (!pid) return false;
  return pid.length === 9 && parseInt(pid, 10) !== NaN;
};

const solve2 = (data) =>
  data.filter(({ byr, iyr, eyr, hgt, hcl, ecl, pid }) => {
    if (!validByr(byr)) {
      return false;
    }
    if (!validIyr(iyr)) {
      return false;
    }
    if (!validEyr(eyr)) {
      return false;
    }
    if (!validHgt(hgt)) {
      return false;
    }
    if (!validHcl(hcl)) {
      return false;
    }
    if (!validEcl(ecl)) {
      return false;
    }
    if (!validPid(pid)) {
      return false;
    }
    return true;
  }).length;

module.exports.validByr = validByr;
module.exports.validHgt = validHgt;
module.exports.validHcl = validHcl;
module.exports.validEcl = validEcl;
module.exports.validPid = validPid;
module.exports.solve1 = solve1;
module.exports.solve2 = solve2;
module.exports.parseData = parseData;
