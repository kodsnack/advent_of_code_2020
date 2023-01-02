import '../util/util.dart';

//const String inputFile = 'day9/example.txt';
const String inputFile = 'day9/input.txt';

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
  List<int> numbers = [];
  for (final line in inputLines) {
    numbers.add(int.parse(line));
  }
  int failNo = findFailingNumber(numbers, 25);
  return failNo;
}

int calcResultP2(List<String> inputLines) {
  List<int> numbers = [];
  for (final line in inputLines) {
    numbers.add(int.parse(line));
  }
  int failNo = findFailingNumber(numbers, 25);
  int result = findP2Result(numbers, failNo);
  return result;
}

int findP2Result(List<int> numbers, int numberToFind) {
  bool found = false;
  int firstIdx = 0;
  int lastIdx = firstIdx + 1;
  while (!found) {
    int sum = numbers[firstIdx];
    while (sum < numberToFind) {
      sum += numbers[lastIdx];
      lastIdx++;
    }
    if (sum == numberToFind) {
      found = true;
    } else {
      firstIdx++;
      lastIdx = firstIdx + 1;
    }
  }
  final sublist = numbers.sublist(firstIdx, lastIdx);
  sublist.sort();
  int result = sublist.first + sublist.last;
  return result;
}

int findFailingNumber(List<int> numbers, int preamble) {
  int idx = preamble;
  while (idx < numbers.length) {
    final preList = numbers.sublist(idx - preamble, idx);
    int nextNumber = numbers[idx];
    bool isValid = false;
    for (int i = 0; i < preList.length - 1; i++) {
      for (int j = i + 1; j < preList.length; j++) {
        if (preList[i] + preList[j] == nextNumber) {
          isValid = true;
          break;
        }
      }
    }
    if (!isValid) return nextNumber;
    idx++;
  }
  return 0;
}
