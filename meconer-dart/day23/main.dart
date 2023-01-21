import '../util/util.dart';

//const String inputFile = 'day23/example.txt';
const String inputFile = 'day23/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

String calcResultP1(String input) {
  Game game = Game();
  game.circle = input.split('').map((e) => int.parse(e)).toList();
  String result = game.play(100);
  return result;
}

int calcResultP2(String input) {
  GameP2 gameP2 = GameP2();
  final inputList = input.split('').map((e) => int.parse(e)).toList();
  gameP2.makeCircle(inputList);
  gameP2.addManyCups(inputList.last, inputList.first, 1000000);
  int resultP2 = gameP2.play(10000000);
  return resultP2;
}

class Game {
  List<int> circle = [];

  int currIdx = 0;
  int currentCup = 0;

  String play(int rounds) {
    // Does rounds moves
    currentCup = circle[currIdx];
    for (int round = 0; round < rounds; round++) {
      List<int> pickedUpCups = pickThreeRight(currIdx);

      // Get next cup with a value 1 less than current cup if possible.
      // Otherwise decrease 1 and wrap around
      int destCup = currentCup - 1;
      if (destCup == 0) destCup = 9;
      int destIdx = 0;
      while (true) {
        destIdx = circle.indexOf(destCup);
        if (destIdx >= 0) break;
        destCup--;
        if (destCup == 0) destCup = 9;
      }
      circle.insertAll(destIdx + 1, pickedUpCups);
      // Locate the current cup again since we changed the circle
      currIdx = circle.indexOf(currentCup);
      currIdx = (currIdx + 1) % circle.length;
      currentCup = circle[currIdx];
    }
    int idxOne = circle.indexOf(1);
    List<int> result = [
      ...circle.sublist(idxOne + 1),
      ...circle.sublist(0, idxOne)
    ];
    String resStr = result.join();
    return resStr;
  }

  List<int> pickThreeRight(int currIdx) {
    List<int> pickedUp = [];
    for (int i = 0; i < 3; i++) {
      int nextIdx = currIdx + 1;
      if (nextIdx >= circle.length) nextIdx = 0;
      pickedUp.add(circle.removeAt(nextIdx));
    }
    return pickedUp;
  }
}

class CircleEntry {
  int idx, nextCup;
  CircleEntry(this.idx, this.nextCup);
}

class GameP2 {
  Map<int, CircleEntry> circle = {};

  int maxCupNo = 0;
  int currentCup = 0;

  void printCircle(int firstPicked, int destCup) {
    int cup = currentCup;
    String line = 'cups: ';
    for (int i = 0; i < circle.length; i++) {
      line += '$cup ';
      cup = circle[cup]!.nextCup;
    }
    print(line);
    line = 'pick up: ';
    cup = firstPicked;
    for (int i = 0; i < 3; i++) {
      line += '$cup ';
      cup = circle[cup]!.nextCup;
    }
    print(line);
    print('destination: $destCup');
    print('');
  }

  void makeCircle(List<int> numbers) {
    for (int i = 0; i < numbers.length - 1; i++) {
      circle[numbers[i]] = CircleEntry(i, numbers[i + 1]);
    }
    circle[numbers[numbers.length - 1]] =
        CircleEntry(numbers.length - 1, numbers[0]);
    currentCup = numbers[0];
    maxCupNo = numbers.length;
  }

  void addManyCups(int lastCup, int firstCup, int totalCups) {
    circle[lastCup]!.nextCup = maxCupNo + 1;
    for (int n = maxCupNo + 1; n <= totalCups; n++) {
      circle[n] = CircleEntry(n, n + 1);
    }
    circle[totalCups]!.nextCup = firstCup;
    maxCupNo = totalCups;
  }

  int play(int rounds) {
    // Does rounds moves
    for (int round = 0; round < rounds; round++) {
      // Pick three cups to the right.
      List<int> pickedUpCups = [];
      pickedUpCups.add(circle[currentCup]!.nextCup);
      pickedUpCups.add(circle[pickedUpCups[0]]!.nextCup);
      pickedUpCups.add(circle[pickedUpCups[1]]!.nextCup);

      // Get next cup with a value 1 less than current cup if possible.
      // Otherwise decrease 1 and wrap around
      int destCup = currentCup - 1;
      if (destCup == 0) destCup = maxCupNo;
      while (pickedUpCups.contains(destCup)) {
        destCup--;
        if (destCup == 0) destCup = maxCupNo;
      }

      // Remove the three picked up cups by setting new nextcup value on current cup.
      circle[currentCup]!.nextCup = circle[pickedUpCups[2]]!.nextCup;

      // Insert the three picked up cups by setting the proper nextCup values
      circle[pickedUpCups[2]]!.nextCup = circle[destCup]!.nextCup;
      circle[destCup]!.nextCup = pickedUpCups[0];
      currentCup = circle[currentCup]!.nextCup;
      // printCircle(pickedUpCups[0], destCup);
    }

    int nextCup = circle[1]!.nextCup;
    int secondNextCup = circle[nextCup]!.nextCup;
    return nextCup * secondNextCup;
  }
}
