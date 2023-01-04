import '../util/util.dart';

//const String inputFile = 'day13/example.txt';
const String inputFile = 'day13/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInput(inputFile);

  // print('Part 1:');
  // final resultP1 = calcResultP1(inputLines);
  // print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

int calcResultP1(List<String> inputLines) {
  int earliestDepTime = int.parse(inputLines[0]);
  List<int> busIds = [];
  inputLines[1].split(',').forEach((id) {
    int busId = int.tryParse(id) ?? -1;
    if (busId > 0) busIds.add(busId);
  });

  int bestId = 0;
  int minDelay = veryLargeNumber;
  busIds.forEach((id) {
    int busDepTime = (earliestDepTime ~/ id) * id;
    if (busDepTime < earliestDepTime) busDepTime += id;
    int delay = busDepTime - earliestDepTime;
    print('ID: $id, delay: $delay ');
    if (delay < minDelay) {
      minDelay = delay;
      bestId = id;
      print('ID: $bestId, delay: $minDelay ');
    }
  });
  return bestId * minDelay;
}

int calcResultP2(List<String> inputLines) {
  List<int> busIds = [];
  List<int> timeDiffs = [];
  int idx = 0;
  inputLines[1].split(',').forEach((id) {
    int busId = int.tryParse(id) ?? -1;
    if (busId > 0) {
      busIds.add(busId);
      timeDiffs.add(idx);
    }
    idx++;
  });

  int stepSize = busIds[0];
  //sum += sumOfDiffs;
  int time = stepSize;

  int busIdx = 1;
  while (busIdx < busIds.length) {
    while ((time + timeDiffs[busIdx]) % busIds[busIdx] != 0) {
      time += stepSize;
    }
    stepSize *= busIds[busIdx];
    busIdx++;
  }
  return time;
}

bool checkTime(int time, List<int> busIds, List<int> timeDiffs) {
  for (int idx = 0; idx < busIds.length; idx++) {
    int id = busIds[idx];
    int depTime = (time ~/ id) * id;
    if (depTime < time) depTime += id;
    int delay = depTime - time;
    if (delay != timeDiffs[idx]) {
      return false;
    }
  }
  return true;
}
