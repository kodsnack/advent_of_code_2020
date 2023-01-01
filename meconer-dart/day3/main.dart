import '../util/linepos.dart';
import '../util/util.dart';

//const String inputFile = 'day3/example.txt';
const String inputFile = 'day3/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);
  print('Part 1:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

int calcResultP1(List<String> inputLines) {
  Forest forest = Forest(inputLines);

  return forest.countTrees(colDelta: 3, rowDelta: 1);
}

int calcResultP2(List<String> inputLines) {
  Forest forest = Forest(inputLines);

  int result1 = forest.countTrees(colDelta: 1, rowDelta: 1);
  int result2 = forest.countTrees(colDelta: 3, rowDelta: 1);
  int result3 = forest.countTrees(colDelta: 5, rowDelta: 1);
  int result4 = forest.countTrees(colDelta: 7, rowDelta: 1);
  int result5 = forest.countTrees(colDelta: 1, rowDelta: 2);

  return result1 * result2 * result3 * result4 * result5;
}

class Forest {
  Set<LinePos> trees = {};
  late int width;
  late int height;

  Forest(List<String> inputLines) {
    width = inputLines[0].length;
    height = inputLines.length;
    for (int row = 0; row < inputLines.length; row++) {
      for (int col = 0; col < inputLines[row].length; col++) {
        if (inputLines[row].substring(col, col + 1) == '#') {
          trees.add(LinePos(col, row));
        }
      }
    }
  }

  int countTrees({required int colDelta, required int rowDelta}) {
    int treeCount = 0;
    int row = 0;
    int col = 0;
    while (row < height) {
      if (trees.contains(LinePos(col % width, row))) treeCount++;
      row += rowDelta;
      col += colDelta;
    }

    return treeCount;
  }
}
