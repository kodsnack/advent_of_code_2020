import '../util/util.dart';

//const String inputFile = 'day22/example.txt';
const String inputFile = 'day22/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

int calcResultP1(String inputLines) {
  List<int> p1Cards = inputLines
      .split('\n\n')[0]
      .split('\n')
      .sublist(1)
      .map((e) => int.parse(e))
      .toList();
  List<int> p2Cards = inputLines
      .split('\n\n')[1]
      .split('\n')
      .sublist(1)
      .map((e) => int.parse(e))
      .toList();

  while (p1Cards.isNotEmpty && p2Cards.isNotEmpty) {
    int p1 = p1Cards.removeAt(0);
    int p2 = p2Cards.removeAt(0);
    if (p1 > p2) p1Cards.addAll([p1, p2]);
    if (p1 < p2) p2Cards.addAll([p2, p1]);
    if (p1 == p2) print('Same cards! What?!');
  }

  int result = 0;
  List<int> winnerCards = p1Cards.isEmpty ? p2Cards : p1Cards;
  while (winnerCards.isNotEmpty) {
    result += winnerCards.length * winnerCards.removeAt(0);
  }
  return result;
}

int calcResultP2(String inputLines) {
  List<int> p1Cards = inputLines
      .split('\n\n')[0]
      .split('\n')
      .sublist(1)
      .map((e) => int.parse(e))
      .toList();
  List<int> p2Cards = inputLines
      .split('\n\n')[1]
      .split('\n')
      .sublist(1)
      .map((e) => int.parse(e))
      .toList();

  int result = playRecursiveCombat(p1Cards, p2Cards);

  return result;
}

int playRecursiveCombat(List<int> p1Cards, List<int> p2Cards) {
  Set<String> gamesAlreadyPlayed = {};
  int winner = 0;
  while (p1Cards.isNotEmpty && p2Cards.isNotEmpty && winner == 0) {
    String gameRepr = p1Cards.join(',') + '-' + p2Cards.join(',');
    if (gamesAlreadyPlayed.contains(gameRepr)) {
      winner = 1;
    } else {
      gamesAlreadyPlayed.add(gameRepr);
      int p1 = p1Cards.removeAt(0);
      int p2 = p2Cards.removeAt(0);

      if (p1 <= p1Cards.length && p2 <= p2Cards.length) {
        List<int> p1CardsCopy = p1Cards.take(p1).toList();
        List<int> p2CardsCopy = p2Cards.take(p2).toList();

        winner = recursiveGame(p1CardsCopy, p2CardsCopy, gamesAlreadyPlayed);

        if (winner == 1) p1Cards.addAll([p1, p2]);
        if (winner == 2) p2Cards.addAll([p2, p1]);
      } else {
        if (p1 > p2) p1Cards.addAll([p1, p2]);
        if (p1 < p2) p2Cards.addAll([p2, p1]);
      }
      winner = 0;
    }
  }

  int result = 0;
  List<int> winnerCards = p2Cards.isEmpty || winner == 1 ? p1Cards : p2Cards;
  while (winnerCards.isNotEmpty) {
    result += winnerCards.length * winnerCards.removeAt(0);
  }
  return result;
}

int recursiveGame(
    List<int> p1Cards, List<int> p2Cards, Set<String> gamesAlreadyPlayed) {
  Set<String> gamesAlreadyPlayed = {};

  int winner = 0;

  while (p1Cards.isNotEmpty && p2Cards.isNotEmpty && winner == 0) {
    String gameRepr = p1Cards.join(',') + '-' + p2Cards.join(',');
    if (gamesAlreadyPlayed.contains(gameRepr)) {
      winner = 1;
    } else {
      gamesAlreadyPlayed.add(gameRepr);
      int p1 = p1Cards.removeAt(0);
      int p2 = p2Cards.removeAt(0);

      if (p1 <= p1Cards.length && p2 <= p2Cards.length) {
        List<int> p1CardsCopy = p1Cards.take(p1).toList();
        List<int> p2CardsCopy = p2Cards.take(p2).toList();
        winner = recursiveGame(p1CardsCopy, p2CardsCopy, gamesAlreadyPlayed);
        if (winner == 1) p1Cards.addAll([p1, p2]);
        if (winner == 2) p2Cards.addAll([p2, p1]);
      } else {
        if (p1 > p2) p1Cards.addAll([p1, p2]);
        if (p1 < p2) p2Cards.addAll([p2, p1]);
      }
      winner = 0;
    }
  }
  // Return the winner
  return p2Cards.isEmpty || winner == 1 ? 1 : 2;
}
