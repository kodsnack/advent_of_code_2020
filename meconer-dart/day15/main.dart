import '../util/util.dart';

//const String inputFile = 'day15/example.txt';
const String inputFile = 'day15/input.txt';

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
  List<int> spokenNumbers =
      inputLines[0].split(',').map((e) => int.parse(e)).toList();
  while (spokenNumbers.length < 2020) {
    int lastNumber = spokenNumbers.last;
    int lastTimeSpokenBefore =
        spokenNumbers.lastIndexOf(lastNumber, spokenNumbers.length - 2);
    if (lastTimeSpokenBefore < 0) {
      spokenNumbers.add(0);
    } else {
      spokenNumbers.add(spokenNumbers.length - 1 - lastTimeSpokenBefore);
    }
  }
  return spokenNumbers.last;
}

int calcResultP2(List<String> inputLines) {
  Map<int, int> spokenNumbers = {};

  final startList = inputLines[0].split(',').map((e) => int.parse(e)).toList();
  for (int i = 0; i < startList.length - 1; i++) {
    spokenNumbers[startList[i]] = i;
  }

  int idx = startList.length - 1;
  int lastNumber = startList.last;
  int nextNumber = 0;
  int limit = 30000000;
  while (true) {
    int? lastTimeSpokenBefore = spokenNumbers[lastNumber];
    if (lastTimeSpokenBefore == null) {
      nextNumber = 0;
    } else {
      nextNumber = idx - lastTimeSpokenBefore;
    }
    spokenNumbers[lastNumber] = idx;
    if (idx >= limit - 1) break;
    lastNumber = nextNumber;
    idx++;
  }
  return lastNumber;
}
