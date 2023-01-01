class Pos {
  final int x, y;
  Pos(this.x, this.y);

  Pos moveUp() {
    return Pos(x, y + 1);
  }

  Pos moveDown() {
    return Pos(x, y - 1);
  }

  Pos moveLeft() {
    return Pos(x - 1, y);
  }

  Pos moveRight() {
    return Pos(x + 1, y);
  }

  Pos moveDir(String command) {
    switch (command) {
      case 'U':
        return moveUp();
      case 'D':
        return moveDown();
      case 'L':
        return moveLeft();
      case 'R':
        return moveRight();
      default:
        throw Exception('Wrong command');
    }
  }

  Pos calcNewTailPos(Pos headPos) {
    int dx = headPos.x - x;
    int dy = headPos.y - y;

    int newTailx = x;
    int newTaily = y;

    if (dx.abs() == 2) {
      // We need to move tail in x
      newTailx += dx.sign;
      // In this case we move y to same as head
      if (dy.abs() == 2) {
        newTaily += dy.sign;
      } else {
        newTaily = headPos.y;
      }
    } else if (dy.abs() == 2) {
      // We need to move tail in x
      newTaily = dy.sign + y;
      // In this case we move y to same as head
      newTailx = headPos.x;
    }
    return Pos(newTailx, newTaily);
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Pos && (x == other.x && y == other.y);
  }

  int get hashCode => x * 100000 + y;

  moveDirWithLimit(String dir, int limitx, int limity) {
    Pos pos = moveDir(dir);
    if (pos.x < 0) return Pos(0, pos.y);
    if (pos.x > limitx) return Pos(limitx, pos.y);
    if (pos.y < 0) return Pos(pos.x, 0);
    if (pos.y > limity) return Pos(pos.x, limity);
    return pos;
  }

  int manhattanDistance(Pos pos) {
    int xDist = (pos.x - x).abs();
    int yDist = (pos.y - y).abs();
    return xDist + yDist;
  }
}
