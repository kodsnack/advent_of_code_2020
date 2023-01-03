import '../util/linepos.dart';
import '../util/util.dart';

//const String inputFile = 'day11/example.txt';
const String inputFile = 'day11/input.txt';

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
  Grid layout = Grid(inputLines);
  //layout.draw();
  bool didChange = true;
  while (didChange) {
    didChange = layout.doRound();
    //layout.draw();
  }
  return layout.countOccupiedSeats();
}

int calcResultP2(List<String> inputLines) {
  Grid layout = Grid(inputLines);
  //layout.draw();
  bool didChange = true;
  while (didChange) {
    didChange = layout.doRoundP2();
    //layout.draw();
  }
  return layout.countOccupiedSeats();
}

class Grid {
  List<List<String>> seats = [];
  Grid(List<String> inputLines) {
    for (final line in inputLines) {
      seats.add(line.split(''));
    }
  }

  int get height => seats.length;

  int get width => seats[0].length;

  void draw() {
    for (final line in seats) {
      print(line.join());
    }
    print(' - ');
  }

  bool doRound() {
    /**
     * 
    If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.

     */
    bool didChange = false;
    List<List<String>> newGrid = buildEmptyGrid(height, width);
    for (int row = 0; row < height; row++) {
      for (int col = 0; col < width; col++) {
        String seat = seats[row][col];
        newGrid[row][col] = seat;

        if (seat != '.') {
          final neighbours = [
            ...LinePos(col, row).getNeighbours(),
            ...LinePos(col, row).getDiagonalNeighbours()
          ];
          neighbours.removeWhere((pos) =>
              pos.col < 0 ||
              pos.col >= width ||
              pos.row < 0 ||
              pos.row >= height);
          int count = 0;
          for (final neighbour in neighbours) {
            if (seats[neighbour.row][neighbour.col] == '#') count++;
          }

          if (count >= 4) {
            newGrid[row][col] = 'L';
            if (seat != 'L') didChange = true;
          }
          if (count == 0) {
            newGrid[row][col] = '#';
            if (seat != '#') didChange = true;
          }
        }
      }
    }
    seats = newGrid;
    return didChange;
  }

  bool doRoundP2() {
    /**
     * 
    If a seat is empty (L) and we can see no occupied seats looking in all 8
    directions, the seat becomes occupied.
    If a seat is occupied (#) and _five_ or more seats adjacent to it are also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.

     */
    bool didChange = false;
    List<List<String>> newGrid = buildEmptyGrid(height, width);

    for (int row = 0; row < height; row++) {
      for (int col = 0; col < width; col++) {
        String seat = seats[row][col];

        if (seat != '.') {
          newGrid[row][col] = seat;

          int count = 0;
          for (int dRow = -1; dRow <= 1; dRow++) {
            for (int dCol = -1; dCol <= 1; dCol++) {
              if (dCol == 0 && dRow == 0) continue;
              bool occupied = findOccupied(LinePos(col, row), dRow, dCol);
              if (occupied) count++;
            }
          }

          if (count >= 5) {
            newGrid[row][col] = 'L';
            if (seat != 'L') didChange = true;
          }
          if (count == 0) {
            newGrid[row][col] = '#';
            if (seat != '#') didChange = true;
          }
        }
      }
    }

    seats = newGrid;
    return didChange;
  }

  List<List<String>> buildEmptyGrid(int height, int width) {
    List<List<String>> grid = [];
    for (int row = 0; row < height; row++) {
      grid.add(List.generate(width, (_) => '.'));
    }
    return grid;
  }

  int countOccupiedSeats() {
    int count = 0;

    for (int row = 0; row < height; row++) {
      for (int col = 0; col < width; col++) {
        if (seats[row][col] == '#') count++;
      }
    }
    return count;
  }

  bool findOccupied(LinePos pos, int dRow, int dCol) {
    int c = pos.col;
    int r = pos.row;
    while (true) {
      c = c + dCol;
      r = r + dRow;
      if (c < 0 || c >= width || r < 0 || r >= height) {
        break;
      }
      if (seats[r][c] == 'L') return false;
      if (seats[r][c] == '#') return true;
    }
    return false;
  }
}
