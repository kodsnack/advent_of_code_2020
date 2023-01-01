import 'dart:math';

import '../util/util.dart';

//const String inputFile = 'day5/example.txt';
const String inputFile = 'day5/input.txt';

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
  int maxSeatId = 0;
  for (final line in inputLines) {
    int row = getRow(line.substring(0, 7));
    int seat = getSeat(line.substring(7));
    int seatId = row * 8 + seat;
    maxSeatId = max(seatId, maxSeatId);
    //print('- $line : row $row, column $seat, seat ID $seatId');
  }

  return maxSeatId;
}

int calcResultP2(List<String> inputLines) {
  List<int> seatIds = [];
  for (final line in inputLines) {
    int row = getRow(line.substring(0, 7));
    int seat = getSeat(line.substring(7));
    int seatId = row * 8 + seat;
    seatIds.add(seatId);
  }
  seatIds.sort();
  return emptySeat(seatIds);
}

int emptySeat(List<int> seatIds) {
  int start = seatIds.removeAt(0);
  while (seatIds.isNotEmpty) {
    int next = seatIds.removeAt(0);
    if (start + 1 != next) return start + 1;
    start = next;
  }
  return -1;
}

int getSeat(String substring) {
  int val = 0;
  for (final char in substring.split('')) {
    val *= 2;
    if (char == 'R') val += 1;
  }
  return val;
}

int getRow(String substring) {
  int val = 0;
  for (final char in substring.split('')) {
    val *= 2;
    if (char == 'B') val += 1;
  }
  return val;
}
