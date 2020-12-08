using System;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt").ToList();
var lineCount = lines.Count();
var lineWidth = lines.First().Length;

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

long PartOne() => GetTreeCount(3, 1);

long PartTwo()
{
  (int right, int down)[] moves = new[] { (1, 1), (3, 1), (5, 1), (7, 1), (1, 2) };

  var treeCounts = moves.Select(x => GetTreeCount(x.right, x.down));
  return treeCounts.Aggregate((total, next) => total * next);
}

long GetTreeCount(int right, int down)
{
  var result = 0;

  for (int i = down, moves = 1; i < lineCount; i = i + down, moves++)
  {
    var index = right * moves % lineWidth;
    var line = lines.ElementAt(i);
    var position = line.ElementAt(index);

    if (position == '#') result++;
  }
  return result;
}
