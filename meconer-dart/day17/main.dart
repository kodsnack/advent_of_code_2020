import 'dart:math';

import '../util/util.dart';

//const String inputFile = 'day17/example.txt';
const String inputFile = 'day17/input.txt';

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
  Space space = Space();
  int height = inputLines.length;
  int width = inputLines[0].length;
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      if (inputLines[height - y - 1].substring(x, x + 1) == '#') {
        space.activeCubes.add(Point3D(x, y, 0));
      }
    }
  }

  //space.draw();
  for (int i = 0; i < 6; i++) {
    space.doRound();
    //space.draw();
  }

  return space.activeCubes.length;
}

int calcResultP2(List<String> inputLines) {
  Space4D space4D = Space4D();
  int height = inputLines.length;
  int width = inputLines[0].length;
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      if (inputLines[height - y - 1].substring(x, x + 1) == '#') {
        space4D.activeCubes.add(Point4D(x, y, 0, 0));
      }
    }
  }

  //space4D.draw();
  for (int i = 0; i < 6; i++) {
    space4D.doRound();
    //space4D.draw();
  }

  return space4D.activeCubes.length;
}

class Range3D {
  int xMin = veryLargeNumber, yMin = veryLargeNumber, zMin = veryLargeNumber;
  int xMax = -veryLargeNumber, yMax = -veryLargeNumber, zMax = -veryLargeNumber;

  void extend(int x, int y, int z) {
    xMin = min(xMin, x);
    yMin = min(yMin, y);
    zMin = min(zMin, z);
    xMax = max(xMax, x);
    yMax = max(yMax, y);
    zMax = max(zMax, z);
  }
}

class Range4D {
  int xMin = veryLargeNumber,
      yMin = veryLargeNumber,
      zMin = veryLargeNumber,
      wMin = veryLargeNumber;
  int xMax = -veryLargeNumber,
      yMax = -veryLargeNumber,
      zMax = -veryLargeNumber,
      wMax = -veryLargeNumber;

  void extend(int x, int y, int z, int w) {
    xMin = min(xMin, x);
    yMin = min(yMin, y);
    zMin = min(zMin, z);
    wMin = min(wMin, w);
    xMax = max(xMax, x);
    yMax = max(yMax, y);
    zMax = max(zMax, z);
    wMax = max(wMax, w);
  }
}

class Space {
  Set<Point3D> activeCubes = {};
  Range3D range3d = Range3D();

  void calcRange() {
    for (final point in activeCubes) {
      range3d.extend(point.x, point.y, point.z);
    }
  }

  void draw() {
    calcRange();
    print('===============');
    for (int z = range3d.zMin; z <= range3d.zMax; z++) {
      print('');
      print('z=$z');
      for (int y = range3d.yMax; y >= range3d.yMin; y--) {
        String line = '';
        for (int x = range3d.xMin; x <= range3d.xMax; x++) {
          line += activeCubes.contains(Point3D(x, y, z)) ? '#' : '.';
        }
        print(line);
      }
    }
  }

  void doRound() {
    calcRange();
    Set<Point3D> nextActiveCubes = {};
    for (int z = range3d.zMin - 1; z <= range3d.zMax + 1; z++) {
      for (int y = range3d.yMin - 1; y <= range3d.yMax + 1; y++) {
        for (int x = range3d.xMin - 1; x <= range3d.xMax + 1; x++) {
          final neighbours = Point3D(x, y, z).neighbours;

          // Count active neighbours
          int countActiveNeighbours = 0;
          for (final neighbour in neighbours) {
            if (activeCubes.contains(neighbour)) countActiveNeighbours++;
          }

          // If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
          if (activeCubes.contains(Point3D(x, y, z))) {
            // Cube is active. Checik if 2 or 3 neighbours are active
            if (countActiveNeighbours == 2 || countActiveNeighbours == 3) {
              nextActiveCubes.add(Point3D(x, y, z));
            }
          } else {
            //  If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
            if (countActiveNeighbours == 3) {
              nextActiveCubes.add(Point3D(x, y, z));
            }
          }
        }
      }
    }
    activeCubes = nextActiveCubes;
  }
}

class Space4D {
  Set<Point4D> activeCubes = {};
  Range4D range4D = Range4D();

  void calcRange() {
    for (final point in activeCubes) {
      range4D.extend(point.x, point.y, point.z, point.w);
    }
  }

  void draw() {
    calcRange();
    print('===============');

    for (var w = range4D.wMin; w <= range4D.wMax; w++) {
      print('');
      for (int z = range4D.zMin; z <= range4D.zMax; z++) {
        print('z=$z, w=$w');
        for (int y = range4D.yMax; y >= range4D.yMin; y--) {
          String line = '';
          for (int x = range4D.xMin; x <= range4D.xMax; x++) {
            line += activeCubes.contains(Point4D(x, y, z, w)) ? '#' : '.';
          }
          print(line);
        }
      }
    }
  }

  void doRound() {
    calcRange();
    Set<Point4D> nextActiveCubes = {};
    for (int w = range4D.wMin - 1; w <= range4D.wMax + 1; w++) {
      for (int z = range4D.zMin - 1; z <= range4D.zMax + 1; z++) {
        for (int y = range4D.yMin - 1; y <= range4D.yMax + 1; y++) {
          for (int x = range4D.xMin - 1; x <= range4D.xMax + 1; x++) {
            final neighbours = Point4D(x, y, z, w).neighbours;

            // Count active neighbours
            int countActiveNeighbours = 0;
            for (final neighbour in neighbours) {
              if (activeCubes.contains(neighbour)) countActiveNeighbours++;
            }

            // If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
            if (activeCubes.contains(Point4D(x, y, z, w))) {
              // Cube is active. Checik if 2 or 3 neighbours are active
              if (countActiveNeighbours == 2 || countActiveNeighbours == 3) {
                nextActiveCubes.add(Point4D(x, y, z, w));
              }
            } else {
              //  If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
              if (countActiveNeighbours == 3) {
                nextActiveCubes.add(Point4D(x, y, z, w));
              }
            }
          }
        }
      }
    }
    activeCubes = nextActiveCubes;
  }
}

class Point3D {
  int x, y, z;
  Point3D(this.x, this.y, this.z);

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Point3D && (x == other.x && y == other.y && z == other.z);
  }

  int get hashCode => (z * 1000000 + y * 1000 + x);

  Set<Point3D> get neighbours {
    Set<Point3D> neighbours = {};
    for (int dx = -1; dx <= 1; dx++) {
      for (int dy = -1; dy <= 1; dy++) {
        for (int dz = -1; dz <= 1; dz++) {
          if (dx != 0 || dy != 0 || dz != 0) {
            neighbours.add(Point3D(x + dx, y + dy, z + dz));
          }
        }
      }
    }
    return neighbours;
  }
}

class Point4D {
  int x, y, z, w;
  Point4D(this.x, this.y, this.z, this.w);

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Point4D &&
        (x == other.x && y == other.y && z == other.z && w == other.w);
  }

  int get hashCode => (w * 1000000000 + z * 1000000 + y * 1000 + x);

  Set<Point4D> get neighbours {
    Set<Point4D> neighbours = {};
    for (int dx = -1; dx <= 1; dx++) {
      for (int dy = -1; dy <= 1; dy++) {
        for (int dz = -1; dz <= 1; dz++) {
          for (int dw = -1; dw <= 1; dw++) {
            if (dx != 0 || dy != 0 || dz != 0 || dw != 0) {
              neighbours.add(Point4D(x + dx, y + dy, z + dz, w + dw));
            }
          }
        }
      }
    }
    return neighbours;
  }
}
