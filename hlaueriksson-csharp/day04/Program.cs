using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

var text = File.ReadAllText("input.txt");
var passports = text.Split("\n\n");
var requiredFields = new[] { "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" };

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne() => GetPassportsWithRequiredFields().Length;
int PartTwo() => GetPassportsWithValidFields().Length;

string[] GetPassportsWithRequiredFields() => passports
  .Where(passport => requiredFields.All(x => passport.Contains(x)))
  .ToArray();

string[] GetPassportsWithValidFields() =>
  GetPassportsWithRequiredFields().Where(IsValidPassport).ToArray();

bool IsValidPassport(string passport)
{
  var fields = passport.Trim().Replace("\n", " ").Split(" ");

  return fields.All(field =>
  {
    var kvp = field.Split(":");
    return IsValidField(key: kvp[0], value: kvp[1]);
  });
}

bool IsValidField(string key, string value)
{
  switch (key)
  {
    case "byr": return IsInRange(value, 1920, 2002);
    case "iyr": return IsInRange(value, 2010, 2020);
    case "eyr": return IsInRange(value, 2020, 2030);
    case "hgt": return IsValidHeight(value);
    case "hcl": return IsValidHairColor(value);
    case "ecl": return IsValidEyeColor(value);
    case "pid": return IsValidPassportId(value);
    default: return true;
  }
}

bool IsInRange(string value, int min, int max)
{
  var number = Convert.ToInt32(value);
  return number >= min && number <= max;
}

bool IsValidHeight(string value)
{
  if (value.EndsWith("cm"))
  {
    return IsInRange(value.Replace("cm", string.Empty), 150, 193);
  }
  if (value.EndsWith("in"))
  {
    return IsInRange(value.Replace("in", string.Empty), 59, 76);
  }
  return false;
}

bool IsValidHairColor(string value) =>
  new Regex("^#[0-9a-f]{6}$").IsMatch(value);

bool IsValidEyeColor(string value) =>
  new[] { "amb", "blu", "brn", "gry", "grn", "hzl", "oth" }.Any(x => value == x);

bool IsValidPassportId(string value) =>
  new Regex("^[0-9]{9}$").IsMatch(value);
