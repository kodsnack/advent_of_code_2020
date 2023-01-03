import '../util/util.dart';

//const String inputFile = 'day10/example.txt';
const String inputFile = 'day10/input.txt';

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
  List<int> jolts = inputLines.map((e) => int.parse(e)).toList();

  jolts.add(0);
  jolts.sort();
  jolts.add(jolts.last + 3);

  int diff3 = 0;
  int diff1 = 0;
  for (int i = 0; i < jolts.length - 1; i++) {
    int diff = jolts[i + 1] - jolts[i];
    if (diff == 1) diff1++;
    if (diff == 3) diff3++;
  }
  return diff1 * diff3;
}

int calcResultP2(List<String> inputLines) {
  List<int> jolts = inputLines.map((e) => int.parse(e)).toList();

  jolts.add(0);
  jolts.sort();
  jolts.add(jolts.last + 3);

  int count = countWays(jolts);

  return count;
}

Map<int, int> calculatedLengths = {};
int countWays(List<int> jolts) {
  if (calculatedLengths.containsKey(jolts.length)) {
    return calculatedLengths[jolts.length]!;
  }
  if (jolts.length <= 2) return 1;
  int diff2 = jolts[2] - jolts.first;
  if (diff2 <= 3) {
    int count1 = countWays(jolts.sublist(1));
    int count2 = countWays([jolts.first, ...jolts.sublist(2)]);
    return count1 + count2;
  } else {
    int count = countWays(jolts.sublist(1));
    calculatedLengths[jolts.length] = count;
    return count;
  }
}
