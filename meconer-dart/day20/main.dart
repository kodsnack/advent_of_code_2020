import '../util/linepos.dart';
import '../util/util.dart';

//const String inputFile = 'day20/example.txt';
const String inputFile = 'day20/input.txt';

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
  final tileLinesList = inputLines.trim().split('\n\n');
  Map<int, Tile> tiles = {};
  for (final tileLines in tileLinesList) {
    Tile tile = Tile(tileLines);
    tiles[tile.tileNo] = tile;
  }

  findCommonEdges(tiles);
  int resultP1 = 1;
  for (final tile in tiles.values) {
    if (tile.commonEdges.length == 2) {
      resultP1 *= tile.tileNo;
    }
  }

  return resultP1;
}

int calcResultP2(String inputLines) {
  final tileLinesList = inputLines.trim().split('\n\n');
  Map<int, Tile> tiles = {};
  for (final tileLines in tileLinesList) {
    Tile tile = Tile(tileLines);
    tiles[tile.tileNo] = tile;
  }

  findCommonEdges(tiles);
  final image = rotateFlipAndOrder(tiles);
  drawImage(image);
  final endImage = buildEndImage(image);
  drawEndImage(endImage);

  int monsterCount = countSeaMonsters(endImage);

  // I was lucky and didnt need to rotate or flip the end image to find the monsters
  // Otherwise it would have been done here. If monsterCount was 0
  drawEndImage(endImage);

  int resultP2 = countWaterRoughness(endImage);

  return resultP2;
}

int countWaterRoughness(List<String> endImage) {
  int counter = 0;
  for (final line in endImage) {
    final re = RegExp('#');
    final matches = re.allMatches(line);
    counter += matches.length;
  }
  return counter;
}

int countSeaMonsters(List<String> endImage) {
  List<String> seaMonster = [
    '                  # ',
    '#    ##    ##    ###',
    ' #  #  #  #  #  #   ',
  ];
  Set<LinePos> monsterCoords = {};
  for (int row = 0; row < seaMonster.length; row++) {
    for (int col = 0; col < seaMonster[row].length; col++) {
      if (seaMonster[row][col] == '#') monsterCoords.add(LinePos(col, row));
    }
  }

  int counter = 0;
  for (int row = 0; row < endImage.length - seaMonster.length; row++) {
    for (int col = 0; col < endImage[0].length - seaMonster[1].length; col++) {
      if (monsterCoords.every((monsterPos) =>
          endImage[monsterPos.row + row][monsterPos.col + col] != '.')) {
        // Found a seamonster
        counter++;
        // Mark the monster
        for (final monsterCoord in monsterCoords) {
          int colToReplace = monsterCoord.col + col;
          endImage[monsterCoord.row + row] = endImage[monsterCoord.row + row]
              .replaceRange(colToReplace, colToReplace + 1, 'O');
        }
        ;
      }
    }
  }
  return counter;
}

void drawEndImage(List<String> endImage) {
  print('---');
  for (final line in endImage) {
    print(line);
  }
}

List<String> buildEndImage(List<List<Tile>> image) {
  List<String> endImage = [];
  for (int tileRow = 0; tileRow < image.length; tileRow++) {
    for (int row = 1; row < image[0][0].tiles.length - 1; row++) {
      String line = '';
      // Remove top and bottom tiles
      for (int tileCol = 0; tileCol < image[0].length; tileCol++) {
        String tileLine = image[tileRow][tileCol].tiles[row];
        line += tileLine.substring(1, tileLine.length - 1);
      }
      endImage.add(line);
    }
  }
  return endImage;
}

void drawImage(List<List<Tile>> image) {
  for (int tileRow = 0; tileRow < image.length; tileRow++) {
    String line = '';
    for (int tileCol = 0; tileCol < image[0].length; tileCol++) {
      int tileNo = image[tileRow][tileCol].tileNo;
      line += 'Tile $tileNo: ';
    }
    print(line);
    for (int row = 0; row < image[0][0].tiles.length; row++) {
      line = '';
      for (int tileCol = 0; tileCol < image[0].length; tileCol++) {
        line += image[tileRow][tileCol].tiles[row] + ' ';
      }
      print(line);
    }
    print(' ');
  }
}

List<List<Tile>> rotateFlipAndOrder(Map<int, Tile> tiles) {
  // Find first corner tile. This will be the top left tile.
  int topLeftCornerTileNo = 0;
  for (final tile in tiles.values) {
    if (tile.commonEdges.length == 2) {
      topLeftCornerTileNo = tile.tileNo;
      break;
    }
  }

  Tile currentTile = tiles[topLeftCornerTileNo]!;

  // Rotate top left corner so edges 1 and 2 are the common edges
  Set<int> commonEdges = currentTile.commonEdges.keys.toSet();
  while (!commonEdges.containsAll([1, 2])) {
    currentTile.rotate(times: 1);
    commonEdges = currentTile.commonEdges.keys.toSet();
  }

  List<List<Tile>> image = [];

  List<Tile> imageLine = [currentTile];
  int rowNo = 0; // Top row
  int colNo = 1;

  // Continue to add new lines while we have a tile south
  while (true) {
    // Add to this line while we have a tile to the right
    while (currentTile.commonEdges.containsKey(1)) {
      currentTile.draw();

      // Rotate and flip the tile to the right so it fits.
      int nextTileNo = currentTile.commonEdges[1]!;
      Tile nextTile = tiles[nextTileNo]!;
      int topTileToMatch = rowNo == 0 ? 0 : image[rowNo - 1][colNo + 1].tileNo;
      int topEdgeValueToMatch =
          rowNo == 0 ? 0 : image[rowNo - 1][colNo + 1].edges[2];
      nextTile.rotateMatchingEdgeToLeft(
          currentTile.tileNo, currentTile.edges[1],
          topTileToMatch: topTileToMatch, topEdgeValue: topEdgeValueToMatch);
      imageLine.add(nextTile);
      currentTile = nextTile;
      currentTile.draw();
      colNo++;
    }
    image.add(imageLine);
    imageLine = [];
    colNo = 0;
    currentTile = image[rowNo]
        [colNo]; // Set the current tile to leftmost tile in row above
    rowNo++;
    if (currentTile.commonEdges.containsKey(2)) {
      Tile nextTile =
          tiles[currentTile.commonEdges[2]]!; // Next tile is the south tile
      nextTile.rotateMatchingEdgeToTop(currentTile.tileNo, currentTile.edges[2],
          rowNo: rowNo, colNo: colNo);
      currentTile = nextTile;
      imageLine.add(currentTile);
      currentTile.draw();
    } else {
      break;
    }
  }
  return image;
}

void findCommonEdges(Map<int, Tile> tiles) {
  for (int i = 0; i < tiles.length - 1; i++) {
    for (int j = i + 1; j < tiles.length; j++) {
      calcCommonEdges(
          tiles.entries.elementAt(i).value, tiles.entries.elementAt(j).value);
    }
  }
}

void calcCommonEdges(Tile tile1, Tile tile2) {
  for (int tile1Edge = 0; tile1Edge < tile1.edges.length; tile1Edge++) {
    for (int tile2Edge = 0; tile2Edge < tile2.edges.length; tile2Edge++) {
      if (tile1.edges[tile1Edge] == tile2.edges[tile2Edge]) {
        tile1.commonEdges[tile1Edge] = tile2.tileNo;
        tile2.commonEdges[tile2Edge] = tile1.tileNo;
      }
      if (tile1.edges[tile1Edge] == tile2.flippedEdges[tile2Edge]) {
        tile1.commonEdges[tile1Edge] = tile2.tileNo;
        tile2.commonEdges[tile2Edge] = tile1.tileNo;
      }
      if (tile1.flippedEdges[tile1Edge] == tile2.edges[tile2Edge]) {
        tile1.commonEdges[tile1Edge] = tile2.tileNo;
        tile2.commonEdges[tile2Edge] = tile1.tileNo;
      }
      if (tile1.flippedEdges[tile1Edge] == tile2.flippedEdges[tile2Edge]) {
        tile1.commonEdges[tile1Edge] = tile2.tileNo;
        tile2.commonEdges[tile2Edge] = tile1.tileNo;
      }
    }
  }
}

class Tile {
  int tileNo = 0;
  /**
   *    --- edge 0 ---
   *    |            |
   *    |            |
   *  3 |            | 1
   *    |            |
   *    |            |
   *    --- edge 2 ---
   */
  List<int> edges = [];
  List<int> flippedEdges = [];
  int rotated = 0;
  bool horFlip = false;
  bool vertFlip = false;

  Map<int, int> commonEdges = {};

  late List<String> tiles;

  Tile(String tileLines) {
    tileNo =
        int.parse(RegExp(r'\d+').firstMatch(tileLines.split('\n')[0])![0]!);
    tiles = tileLines.split('\n').sublist(1);
    calcEdges();
  }

  void draw() {
    print('----------');
    print('Tile $tileNo:');
    for (final tile in tiles) {
      print(tile);
    }
  }

  void calcEdges() {
    // Edge 0
    addEdgesFrom(tiles.first);
    // Edge 1
    addEdgesFrom(getRightEdge());
    // Edge 2
    addEdgesFrom(tiles.last);
    // Edge 3
    addEdgesFrom(getLeftEdge());
  }

  void addEdgesFrom(String s) {
    String binary = s.replaceAll('#', '1');
    binary = binary.replaceAll('.', '0');
    edges.add(int.parse(binary, radix: 2));
    String reversedBinary = binary.split('').reversed.join();
    flippedEdges.add(int.parse(reversedBinary, radix: 2));
  }

  String getLeftEdge() {
    String s = '';
    for (final tile in tiles) {
      s += tile.substring(0, 1);
    }
    return s;
  }

  String getRightEdge() {
    String s = '';
    for (final tile in tiles) {
      s += tile.substring(tile.length - 1);
    }
    return s;
  }

  void rotate({required int times}) {
    for (int i = 0; i < times; i++) {
      // Rotate the tiles
      List<String> newTiles = [];
      for (int c = 0; c < tiles.length; c++) {
        String tileLine = '';
        for (int r = tiles.length - 1; r >= 0; r--) {
          tileLine += tiles[r].substring(c, c + 1);
        }
        newTiles.add(tileLine);
      }
      tiles = newTiles;

      /* Rotate the edges
    Since edges are calculated from left to right and top to bottom we
    have to do some flipping too 
    */

      List<int> newEdges = [0, 0, 0, 0];
      List<int> newFlippedEdges = [0, 0, 0, 0];
      // Edge 0 (N) will be at edge 1 (E). No flip.
      newEdges[1] = edges[0];
      newFlippedEdges[1] = flippedEdges[0];

      // Edge 1 (E) will be at edge 2 (S) flipped.
      newEdges[2] = flippedEdges[1];
      newFlippedEdges[2] = edges[1];

      // Edge 2 (S) will be at edge 3 (W) not flipped.
      newEdges[3] = edges[2];
      newFlippedEdges[3] = flippedEdges[2];

      // Edge 3 (W) will be at edge 0 (N) flipped.
      newEdges[0] = flippedEdges[3];
      newFlippedEdges[0] = edges[3];

      edges = newEdges;
      flippedEdges = newFlippedEdges;

      // And we have to move the common edges accordingly
      Map<int, int> newCommonEdges = {};
      for (final edgeKey in commonEdges.keys) {
        int newKey = (edgeKey + 1) % 4;
        newCommonEdges[newKey] = commonEdges[edgeKey]!;
      }
      commonEdges = newCommonEdges;
    }
  }

  void rotateMatchingEdgeToLeft(int tileToMatch, int edgeValue,
      {required int topTileToMatch, required int topEdgeValue}) {
    int edgeNo = -1;
    for (final edge in commonEdges.entries) {
      if (edge.value == tileToMatch) {
        edgeNo = edge.key;
        break;
      }
    }

    if (edgeNo < 0) print('!! Should not be possible');

    rotate(times: 3 - edgeNo); // Rotate this edge to edge 3 (left)

    if (topTileToMatch == 0) {
      // We are on top row so we check that the edge on top is empty.
      if (commonEdges.containsKey(0)) flipVertical();
    } else {
      // Not on top row. We need to match the tile upwards too.
      if (commonEdges[0] != topTileToMatch) {
        flipVertical();
      }
      if (flippedEdges.contains(topEdgeValue)) flipHorizontal();
    }

    if (flippedEdges.contains(edgeValue)) flipVertical();
  }

  void flipVertical() {
    // Flip the tiles
    tiles = tiles.reversed.toList();

    /* Replace top and bottom edges and flip the left and right edges
    */

    List<int> newEdges = [0, 0, 0, 0];
    List<int> newFlippedEdges = [0, 0, 0, 0];
    // Edge 0 (N) will be at edge 2 (S). No flip.
    newEdges[2] = edges[0];
    newFlippedEdges[2] = flippedEdges[0];

    // Edge 1 (E) will remain at edge 1 (E) but flipped.
    newEdges[1] = flippedEdges[1];
    newFlippedEdges[1] = edges[1];

    // Edge 2 (S) will be at edge 0 (N). No flip.
    newEdges[0] = edges[2];
    newFlippedEdges[0] = flippedEdges[2];

    // Edge 3 (W) will remain at edge 3 (W) but flipped.
    newEdges[3] = flippedEdges[3];
    newFlippedEdges[3] = edges[3];

    edges = newEdges;
    flippedEdges = newFlippedEdges;

    // And we have to move the common edges accordingly
    Map<int, int> newCommonEdges = {};
    for (final edgeKey in commonEdges.keys) {
      int newKey = edgeKey;
      if (edgeKey == 0) newKey = 2;
      if (edgeKey == 2) newKey = 0;
      newCommonEdges[newKey] = commonEdges[edgeKey]!;
    }
    commonEdges = newCommonEdges;
  }

  void flipHorizontal() {
    // Flip the tiles
    List<String> newTiles = [];
    for (final tile in tiles) {
      String newTile = tile.split('').reversed.join();
      newTiles.add(newTile);
    }

    tiles = newTiles;

    /* Replace left and right edges and flip the top and bottom edges
    */

    List<int> newEdges = [0, 0, 0, 0];
    List<int> newFlippedEdges = [0, 0, 0, 0];
    // Edge 0 (N) will remain at edge 0 (N) but flipped.
    newEdges[0] = flippedEdges[0];
    newFlippedEdges[0] = edges[0];

    // Edge 1 (E) will be at edge 3 (W). No flip.
    newEdges[3] = edges[1];
    newFlippedEdges[3] = flippedEdges[1];

    // Edge 2 (S) will remain at edge 2 (N) but flipped.
    newEdges[2] = flippedEdges[2];
    newFlippedEdges[2] = edges[2];

    // Edge 3 (W) will be at edge 1 (E). Not flipped.
    newEdges[1] = edges[3];
    newFlippedEdges[1] = flippedEdges[3];

    edges = newEdges;
    flippedEdges = newFlippedEdges;

    // And we have to move the common edges accordingly
    Map<int, int> newCommonEdges = {};
    for (final edgeKey in commonEdges.keys) {
      int newKey = edgeKey;
      if (edgeKey == 1) newKey = 3;
      if (edgeKey == 3) newKey = 1;
      newCommonEdges[newKey] = commonEdges[edgeKey]!;
    }
    commonEdges = newCommonEdges;
  }

  void rotateMatchingEdgeToTop(int tileToMatch, int edgeValue,
      {required int rowNo, required int colNo}) {
    int edgeNo = -1;
    for (final edge in commonEdges.entries) {
      if (edge.value == tileToMatch) {
        edgeNo = edge.key;
        break;
      }
    }

    if (edgeNo < 0) print('!! Should not be possible');

    int timesToRotate = (4 - edgeNo) % 4;
    rotate(times: timesToRotate); // Rotate this edge to edge 0 (up)

    if (colNo == 0) {
      // We are on leftmost column row so we check that the edge to left is empty.
      if (commonEdges.containsKey(3)) flipHorizontal();
    }

    if (flippedEdges.contains(edgeValue)) flipHorizontal();
  }
}
