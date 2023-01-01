import 'dart:math';

import 'pos.dart';
import 'util.dart';

class Range {
  int xMin = veryLargeNumber;
  int xMax = -veryLargeNumber;
  int yMin = veryLargeNumber;
  int yMax = -veryLargeNumber;

  void extend(Pos point) {
    xMin = min(xMin, point.x);
    xMax = max(xMax, point.x);
    yMin = min(yMin, point.y);
    yMax = max(yMax, point.y);
  }
}
