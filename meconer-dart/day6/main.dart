import '../util/util.dart';

//const String inputFile = 'day6/example.txt';
const String inputFile = 'day6/input.txt';

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
  int sum = 0;
  Set<String> questions = {};
  for (final line in inputLines) {
    if (line.isEmpty) {
      sum += questions.length;
      questions = {};
    } else {
      questions.addAll(line.split(''));
    }
  }
  sum += questions.length;
  return sum;
}

int calcResultP2(List<String> inputLines) {
  int sum = 0;
  Set<String> questions = {};
  bool newGroup = true;
  for (final line in inputLines) {
    if (line.isEmpty) {
      sum += questions.length;
      questions = {};
      newGroup = true;
    } else {
      Set<String> nextPersonsQuestions = {};
      nextPersonsQuestions.addAll(line.split(''));
      if (newGroup) {
        questions = nextPersonsQuestions;
        newGroup = false;
      } else {
        questions = questions.intersection(nextPersonsQuestions);
      }
    }
  }
  sum += questions.length;
  return sum;
}

class Person {
  Set<String> questions = {};
}
