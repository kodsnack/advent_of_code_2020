import '../util/util.dart';

//const String inputFile = 'day2/example.txt';
const String inputFile = 'day2/input.txt';

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
  for (final line in inputLines) {
    Validation validation = Validation(line);
    if (validation.isValid()) count++;
  }
  return count;
}

int calcResultP2(List<String> inputLines) {
  int count = 0;
  for (final line in inputLines) {
    Validation validation = Validation(line);
    if (validation.isValidP2()) count++;
  }
  return count;
}

class Validation {
  late int min, max;
  late String letter;
  late String pwd;

  Validation(String line) {
    min = int.parse(line.split('-')[0]);
    max = int.parse(line.split('-')[1].split(' ')[0]);
    letter = line.split(':')[0].split(' ')[1];
    pwd = line.split(':')[1].trim();
  }

  bool isValid() {
    int letterCount = pwd.split('').where((char) => char == letter).length;
    return letterCount >= min && letterCount <= max;
  }

  bool isValidP2() {
    return (pwd.split('').elementAt(min - 1) == letter) ^
        (pwd.split('').elementAt(max - 1) == letter);
  }
}
