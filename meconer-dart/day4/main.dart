import '../util/util.dart';

//const String inputFile = 'day4/example.txt';
const String inputFile = 'day4/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

int calcResultP1(List<String> inputLines) {
  Passport passport = Passport();
  int validPPCount = 0;
  for (final line in inputLines) {
    if (line.isEmpty) {
      if (passport.isValid()) validPPCount++;
      passport = Passport();
    } else {
      passport.addFields(line);
    }
  }
  if (passport.isValid()) validPPCount++;

  return validPPCount;
}

int calcResultP2(List<String> inputLines) {
  Passport passport = Passport();
  int validPPCount = 0;
  for (final line in inputLines) {
    if (line.isEmpty) {
      if (passport.isValidP2()) validPPCount++;
      passport = Passport();
    } else {
      passport.addFields(line);
    }
  }
  if (passport.isValidP2()) validPPCount++;

  return validPPCount;
}

class Passport {
  Set<String> fields = {};
  Map<String, String> dataFields = {};

  bool isValid() {
    Set<String> needed = {
      'byr',
      'iyr',
      'eyr',
      'hgt',
      'hcl',
      'ecl',
      'pid',
    };
    return dataFields.keys.toSet().containsAll(needed);
  }

  bool isValidP2() {
    if (!isValid()) return false;

    // cid (Country ID) - ignored, missing or not.

    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    if (!validYear(dataFields['byr'], 1920, 2002)) return false;

    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    if (!validYear(dataFields['iyr'], 2010, 2020)) return false;

    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    if (!validYear(dataFields['eyr'], 2020, 2030)) return false;

    // hgt (Height) - a number followed by either cm or in:
    //     If cm, the number must be at least 150 and at most 193.
    //     If in, the number must be at least 59 and at most 76.
    if (!validHeight(dataFields['hgt'])) return false;

    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    if (!validHairColor(dataFields['hcl'])) return false;

    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    if (!validEyeColor(dataFields['ecl'])) return false;

    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    if (!validPid(dataFields['pid'])) return false;

    return true;
  }

  void addFields(String line) {
    final lineParts = line.split(' ');
    for (final part in lineParts) {
      String key = part.split(':')[0];
      fields.add(key);
      dataFields[key] = part.split(':')[1];
    }
  }

  bool validHeight(String? heightStr) {
    if (heightStr == null) return false;
    int cmPos = heightStr.indexOf('cm');
    if (cmPos > 0) {
      int heightInCm = int.tryParse(heightStr.substring(0, cmPos)) ?? 0;
      return heightInCm >= 150 && heightInCm <= 193;
    }

    int inPos = heightStr.indexOf('in');
    if (inPos > 0) {
      int heightInInch = int.tryParse(heightStr.substring(0, inPos)) ?? 0;
      return heightInInch >= 59 && heightInInch <= 76;
    }
    return false;
  }

  bool validHairColor(String? hclStr) {
    if (hclStr == null) return false;
    if (!hclStr.startsWith('#')) return false;
    if (hclStr.length != 7) return false;
    final validChars = '0123456789abcdef'.split('');
    return hclStr
        .substring(1)
        .split('')
        .every((char) => validChars.contains(char));
  }

  bool validEyeColor(String? eclStr) {
    if (eclStr == null) return false;
    final validColors = 'amb blu brn gry grn hzl oth'.split(' ');
    return validColors.contains(eclStr);
  }

  bool validPid(String? pidStr) {
    if (pidStr == null) return false;
    if (pidStr.length != 9) return false;
    final validChars = '0123456789'.split('');
    return pidStr.split('').every((char) => validChars.contains(char));
  }
}

bool validYear(String? yearStr, int min, int max) {
  if (yearStr == null) return false;
  int year = int.tryParse(yearStr) ?? -double.infinity.toInt();
  return year >= min && year <= max;
}
