import '../util/util.dart';

//const String inputFile = 'day7/example2.txt';
const String inputFile = 'day7/input.txt';

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
  Map<String, List<String>> bags = {};
  for (final line in inputLines) {
    String bagName = line.split('contain')[0].split(' bag')[0];
    List<String> contained = line.split('contain')[1].split(',');
    List<String> containedBags = [];
    bags[bagName] = containedBags;
    for (final containedPart in contained) {
      if (!containedPart.contains('no other')) {
        String containedBag =
            containedPart.trim().split(' ').sublist(1, 3).join(' ');
        containedBags.add(containedBag);
      }
    }
  }

  int sum = 0;
  for (final bag in bags.keys) {
    if (canContain(bag, bags, 'shiny gold')) sum++;
  }

  return sum;
}

int calcResultP2(List<String> inputLines) {
  Map<String, List<String>> bags = {};
  for (final line in inputLines) {
    String bagName = line.split('contain')[0].split(' bag')[0];
    List<String> contained = line.split('contain')[1].split(',');
    List<String> containedBags = [];
    bags[bagName] = containedBags;
    for (final containedPart in contained) {
      if (!containedPart.contains('no other')) {
        String containedBag =
            containedPart.trim().split(' ').sublist(0, 3).join(' ');
        containedBags.add(containedBag);
      }
    }
  }

  int count = countContained('shiny gold', bags);

  return count;
}

bool canContain(
    String bag, Map<String, List<String>> bags, String bagNameToFind) {
  for (final containedBag in bags[bag]!) {
    if (containedBag == bagNameToFind) return true;
    if (canContain(containedBag, bags, bagNameToFind)) return true;
  }
  return false;
}

int countContained(String bag, Map<String, List<String>> bags) {
  int count = 0;
  for (final containedBag in bags[bag]!) {
    int number = int.tryParse(containedBag.split(' ')[0]) ?? 0;
    int numberContained =
        countContained(containedBag.split(' ').sublist(1, 3).join(' '), bags);
    count += number + number * numberContained;
  }
  return count;
}
