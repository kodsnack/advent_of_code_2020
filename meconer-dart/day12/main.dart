import '../util/linepos.dart';
import '../util/pos.dart';
import '../util/util.dart';

//const String inputFile = 'day12/example.txt';
const String inputFile = 'day12/input.txt';

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
  Ship ship = Ship();
  for (final line in inputLines) {
    ship.doCommand(line);
  }
  return ship.pos.manhattanDistance(Pos(0, 0));
}

int calcResultP2(List<String> inputLines) {
  Ship ship = Ship();
  for (final line in inputLines) {
    ship.doCommandP2(line);
  }
  return ship.pos.manhattanDistance(Pos(0, 0));
}

class Ship {
  Direction direction = Direction.Right;
  Pos pos = Pos(0, 0);
  Pos waypointPos = Pos(10, 1);

  void doCommand(String line) {
    String command = line.substring(0, 1);
    int value = int.parse(line.substring(1));
    switch (command) {
      case 'N':
        pos = Pos(pos.x, pos.y + value);
        break;
      case 'S':
        pos = Pos(pos.x, pos.y - value);
        break;
      case 'W':
        pos = Pos(pos.x - value, pos.y);
        break;
      case 'E':
        pos = Pos(pos.x + value, pos.y);
        break;
      case 'F':
        moveDir(direction, value);
        break;
      case 'L':
        turnLeft(value);
        break;
      case 'R':
        turnRight(value);
        break;
    }
  }

  void doCommandP2(String line) {
    String command = line.substring(0, 1);
    int value = int.parse(line.substring(1));
    switch (command) {
      case 'N':
        waypointPos = Pos(waypointPos.x, waypointPos.y + value);
        break;
      case 'S':
        waypointPos = Pos(waypointPos.x, waypointPos.y - value);
        break;
      case 'W':
        waypointPos = Pos(waypointPos.x - value, waypointPos.y);
        break;
      case 'E':
        waypointPos = Pos(waypointPos.x + value, waypointPos.y);
        break;
      case 'F':
        moveToWaypoint(value);
        break;
      case 'L':
        turnWPLeft(value);
        break;
      case 'R':
        turnWPRight(value);
        break;
    }
  }

  void moveDir(Direction direction, int value) {
    switch (direction) {
      case Direction.Up:
        pos = Pos(pos.x, pos.y + value);
        break;
      case Direction.Down:
        pos = Pos(pos.x, pos.y - value);
        break;
      case Direction.Left:
        pos = Pos(pos.x - value, pos.y);
        break;
      case Direction.Right:
        pos = Pos(pos.x + value, pos.y);
        break;
    }
  }

  void turnLeft(int value) {
    int turns = value ~/ 90;
    for (int i = 0; i < turns; i++) {
      switch (direction) {
        case Direction.Up:
          direction = Direction.Left;
          break;
        case Direction.Left:
          direction = Direction.Down;
          break;
        case Direction.Down:
          direction = Direction.Right;
          break;
        case Direction.Right:
          direction = Direction.Up;
          break;
      }
    }
  }

  void turnRight(int value) {
    int turns = value ~/ 90;
    for (int i = 0; i < turns; i++) {
      switch (direction) {
        case Direction.Up:
          direction = Direction.Right;
          break;
        case Direction.Left:
          direction = Direction.Up;
          break;
        case Direction.Down:
          direction = Direction.Left;
          break;
        case Direction.Right:
          direction = Direction.Down;
          break;
      }
    }
  }

  void moveToWaypoint(int value) {
    int dx = waypointPos.x;
    int dy = waypointPos.y;
    pos = Pos(pos.x + dx * value, pos.y + dy * value);
  }

  void turnWPRight(int value) {
    int turns = value ~/ 90;
    for (int i = 0; i < turns; i++) {
      waypointPos = Pos(waypointPos.y, -waypointPos.x);
    }
  }

  void turnWPLeft(int value) {
    int turns = value ~/ 90;
    for (int i = 0; i < turns; i++) {
      waypointPos = Pos(-waypointPos.y, waypointPos.x);
    }
  }
}
