import 'dart:math';

import '../util/util.dart';

//const String inputFile = 'day14/example.txt';
const String inputFile = 'day14/input.txt';

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
  late Mask mask;
  Map<int, int> memory = {};
  for (final line in inputLines) {
    final parts = line.split(' = ');

    if (parts[0] == 'mask') mask = Mask(parts[1]);

    if (parts[0].startsWith('mem')) {
      int adress = int.parse(parts[0].split('[')[1].split(']')[0]);
      int value = int.parse(parts[1]);
      value = mask.mask(value);
      memory[adress] = value;
    }
  }
  int sum = 0;
  for (final val in memory.values) {
    sum += val;
  }
  return sum;
}

int calcResultP2(List<String> inputLines) {
  late String mask;
  Map<int, int> memory = {};
  for (final line in inputLines) {
    final parts = line.split(' = ');

    if (parts[0] == 'mask') mask = parts[1];

    if (parts[0].startsWith('mem')) {
      int adress = int.parse(parts[0].split('[')[1].split(']')[0]);
      int value = int.parse(parts[1]);
      writeToMemory(adress, value, mask, memory);
    }
  }
  int sum = 0;
  for (final val in memory.values) {
    sum += val;
  }
  return sum;
}

void writeToMemory(int adress, int value, String mask, Map<int, int> memory) {
  String adressStr = adress.toRadixString(2);
  adressStr = adressStr.padLeft(36, '0');
  List<int> adresses = getAdresses(adressStr, mask);
  adresses.forEach((adr) {
    memory[adr] = value;
  });
}

List<int> getAdresses(String adressStr, String mask) {
  if (mask.isEmpty) return [0];

  List<int> adresses = [];
  final subAdresses = getAdresses(adressStr.substring(1), mask.substring(1));

  if (mask.startsWith('0')) {
    // adress bit is unchanged
    if (adressStr.startsWith('1')) {
      int valToAdd = pow(2, mask.length - 1) as int;
      adresses.addAll(subAdresses.map((e) => e + valToAdd).toList());
    } else {
      adresses.addAll(subAdresses);
    }
  }

  if (mask.startsWith('1')) {
    // adress bit is set to 1
    int valToAdd = pow(2, mask.length - 1) as int;
    adresses.addAll(subAdresses.map((e) => e + valToAdd).toList());
  }

  if (mask.startsWith('X')) {
    // adress bit is "floating". It is both 0 and 1
    int valToAdd = pow(2, mask.length - 1) as int;
    adresses.addAll(subAdresses.map((e) => e + valToAdd).toList());
    adresses.addAll(subAdresses);
  }

  return adresses;
}

class Mask {
  int andMask = int.parse(List.generate(36, (_) => 1).join(), radix: 2);
  int orMask = int.parse(List.generate(36, (_) => 0).join(), radix: 2);

  Mask(String maskStr) {
    String andMaskStr = maskStr;
    andMaskStr = andMaskStr.replaceAll('X', '1');
    andMask = int.parse(andMaskStr.split('').join(), radix: 2);

    String orMaskStr = maskStr;
    orMaskStr = orMaskStr.replaceAll('X', '0');
    orMask = int.parse(orMaskStr.split('').join(), radix: 2);
  }

  int mask(int value) {
    value = value & andMask;
    value = value | orMask;
    return value;
  }
}
