import 'dart:io';

Future<List<String>> readInput(String fileName) {
  final file = File(fileName);
  return file.readAsLines();
}

const int veryLargeNumber = 99999999999999;

bool isDigit(String s, int idx) => (s.codeUnitAt(idx) ^ 0x30) <= 9;
