import '../util/util.dart';

//const String inputFile = 'day19/example.txt';
const String inputFile = 'day19/input.txt';

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
  int count = 0;

  // Build rule list

  Tester tester = Tester(buildRules(inputLines));
  tester.buildRegex();

  bool startOfTestLinesFound = false;

  for (final line in inputLines) {
    if (startOfTestLinesFound) {
      // Test line
      final regExp = RegExp(tester.regex);
      if (regExp.hasMatch(line)) count++;
    } else {
      if (line.isEmpty) startOfTestLinesFound = true;
    }
  }

  // Check the pattern
  return count;
}

int calcResultP2(List<String> inputLines) {
  // Build rule list

  Tester tester = Tester(buildRules(inputLines));

  // Rule rule8 = Rule();
  // rule8.firstPattern = [42];
  // rule8.secondPattern = [42, 8];
  // tester.rules[8] = rule8;

  // Rule rule11 = Rule();
  // rule11.firstPattern = [42, 31];
  // rule11.secondPattern = [42, 11, 31];

  /**
   *  Rule 8 and 11 are now loops so we cannot build a complete regex. 
   * Rule 8 means that we get at least 1 of pattern 42
   * Rule 11 means that we get at least one sequence of 42,31. The number of 42:s
   * and 31:s are equal
   * 
   * Since rule 0 is 8 11 we match strings where the number of 42 seqs will be
   * at least 1 more than the number of 31 seqs. 
  */

  tester.buildRegexP2();

  bool startOfTestLinesFound = false;
  final regExp42 = RegExp(tester.regex42);

  final regExp31 = RegExp(tester.regex31);

  int count = 0;

  for (final line in inputLines) {
    String s = line;
    if (startOfTestLinesFound) {
      // Test line
      int count42 = 0;

      var match42 = regExp42.stringMatch(s);
      while (match42 != null) {
        s = s.substring(match42.length);
        count42++;
        match42 = regExp42.stringMatch(s);
      }

      int count31 = 0;
      var match31 = regExp31.stringMatch(s);
      while (match31 != null) {
        s = s.substring(match31.length);
        count31++;
        match31 = regExp31.stringMatch(s);
      }

      if (s.isEmpty && count42 > count31 && count31 > 0) count++;
    } else {
      if (line.isEmpty) startOfTestLinesFound = true;
    }
  }

  // Check the pattern
  return count;
}

class Tester {
  Map<int, Rule> rules;
  String regex = '';
  String regex42 = '';
  String regex31 = '';

  Tester(this.rules);

  void buildRegex() {
    final rootRule = rules[0]!;
    regex = '';
    for (int pattern in rootRule.firstPattern) {
      regex += getRegexp(rules[pattern]!);
    }
    regex = '^' + regex + '\$';
  }

  String getRegexp(Rule rule) {
    if (rule.possibleString.isNotEmpty) return rule.possibleString;
    String regex = '';
    for (int pattern in rule.firstPattern) {
      regex += getRegexp(rules[pattern]!);
    }
    if (rule.secondPattern.isNotEmpty) {
      regex = '(' + regex;
      regex += '|';
      for (int pattern in rule.secondPattern) {
        regex += getRegexp(rules[pattern]!);
      }
      regex += ')';
    }
    rule.possibleString = regex;
    return regex;
  }

  void buildRegexP2() {
    regex42 = '';
    for (int pattern in rules[42]!.firstPattern) {
      regex42 += getRegexp(rules[pattern]!);
    }
    regex42 += '|';
    for (int pattern in rules[42]!.secondPattern) {
      regex42 += getRegexp(rules[pattern]!);
    }
    regex42 = '^(' + regex42 + ')';

    regex31 = '';
    for (int pattern in rules[31]!.firstPattern) {
      regex31 += getRegexp(rules[pattern]!);
    }
    regex31 += '|';
    for (int pattern in rules[31]!.secondPattern) {
      regex31 += getRegexp(rules[pattern]!);
    }
    regex31 = '^(' + regex31 + ')';
  }
}

Map<int, Rule> buildRules(List<String> inputLines) {
  Map<int, Rule> rules = {};
  for (final line in inputLines) {
    if (line.isEmpty) break;
    int ruleNumber = int.parse(line.split(':')[0]);

    Rule rule = Rule();
    if (line.contains('"')) {
      // This was an end rule. a or b
      if (line.contains('a')) rule.possibleString = "a";
      if (line.contains('b')) rule.possibleString = "b";
    }

    List<String> patterns = line.split(':')[1].split('|');
    rule.firstPattern = getPattern(patterns[0]);
    if (patterns.length > 1) {
      rule.secondPattern = getPattern(patterns[1]);
    }

    rules[ruleNumber] = rule;
  }
  return rules;
}

List<int> getPattern(String s) {
  if (s.isEmpty) return [];
  List<int> intPattern =
      s.trim().split(' ').map((e) => int.tryParse(e) ?? -1).toList();
  return intPattern;
}

class Rule {
  List<int> firstPattern = [];
  List<int> secondPattern = [];
  String possibleString = '';
}
