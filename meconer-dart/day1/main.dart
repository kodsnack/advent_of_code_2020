import '../util/util.dart';

//const String inputFile = 'day1/example.txt';
const String inputFile = 'day1/input.txt';

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
  List<int> input = inputLines.map((e) => int.parse(e)).toList();
  for (int i = 0; i < input.length - 1; i++) {
    for (int j = i + 1; j < input.length; j++) {
      if (input[i] + input[j] == 2020) return input[i] * input[j];
    }
  }
  return 0;
}

int calcResultP2(List<String> inputLines) {
  List<int> input = inputLines.map((e) => int.parse(e)).toList();
  for (int i = 0; i < input.length - 2; i++) {
    for (int j = i + 1; j < input.length - 1; j++) {
      for (int k = j + 1; k < input.length; k++)
        if (input[i] + input[j] + input[k] == 2020)
          return input[i] * input[j] * input[k];
    }
  }
  return 0;
}
