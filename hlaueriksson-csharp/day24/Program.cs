using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");
var paths = lines.Select(x => new Path(x)).ToArray();

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne() => GetGrid().Where(x => x.Value).Count();

int PartTwo()
{
  var grid = GetGrid();

  for (int i = 0; i < 100; i++)
  {
    var next = Run(grid);
    Copy(next, grid);
  }

  return grid.Where(x => x.Value).Count();

  Dictionary<(int x, int y), bool> Run(Dictionary<(int x, int y), bool> grid)
  {
    var next = new Dictionary<(int x, int y), bool>();
    Copy(grid, next);

    foreach (var key in next.Keys)
      AddNeighborsToGrid(GetNeighbors(key.x, key.y));

    foreach (var key in grid.Keys)
      Flip(key.x, key.y);

    return next;

    void Flip(int x, int y)
    {
      var neighbors = GetNeighbors(x, y);
      var blackNeighborsCount = neighbors.Count(n => IsBlack(n.x, n.y));

      if (IsBlack(x, y) && (blackNeighborsCount == 0 || blackNeighborsCount > 2))
        next[(x, y)] = false;
      else if (IsWhite(x, y) && blackNeighborsCount == 2)
        next[(x, y)] = true;
    }

    (int x, int y)[] GetNeighbors(int x, int y)
    {
      return new (int, int)[]
      {
        Neighbor(x, y, Direction.e),
        Neighbor(x, y, Direction.se),
        Neighbor(x, y, Direction.sw),
        Neighbor(x, y, Direction.w),
        Neighbor(x, y, Direction.nw),
        Neighbor(x, y, Direction.ne)
      };
    }

    void AddNeighborsToGrid((int x, int y)[] neighbors)
    {
      foreach (var n in neighbors)
        if (!grid.ContainsKey((n.x, n.y)))
          grid[(n.x, n.y)] = false;
    }

    bool IsWhite(int x, int y) => grid[(x, y)] == false;

    bool IsBlack(int x, int y) => grid.ContainsKey((x, y)) ? grid[(x, y)] : false;
  }

  void Copy(IDictionary from, IDictionary to)
  {
    foreach (var key in from.Keys)
      to[key] = from[key];
  }

  (int x, int y) Neighbor(int x, int y, Direction direction)
  {
    var step = Step(direction);
    return new(x + step.dx, y + step.dy);
  }
}

Dictionary<(int x, int y), bool> GetGrid()
{
  var result = new Dictionary<(int x, int y), bool>();

  foreach (var path in paths)
  {
    (int x, int y) hex = (0, 0);
    foreach (var direction in path.Directions)
    {
      var step = Step(direction);
      hex.x += step.dx;
      hex.y += step.dy;
    }

    if (result.ContainsKey(hex))
      result[hex] = !result[hex];
    else
      result.Add(hex, true);
  }

  return result;
}

(int dx, int dy) Step(Direction direction)
{
  switch (direction)
  {
    case Direction.e: return (2, 0);
    case Direction.se: return (1, 1);
    case Direction.sw: return (-1, 1);
    case Direction.w: return (-2, 0);
    case Direction.nw: return (-1, -1);
    case Direction.ne: return (1, -1);
    default: throw new Exception("💥");
  }
}

enum Direction { e, se, sw, w, nw, ne }

class Path
{
  public List<Direction> Directions { get; } = new List<Direction>();

  public Path(string line)
  {
    while (line.Any())
      Directions.Add(NextDirection());

    Direction NextDirection()
    {
      foreach (var direction in (Direction[])Enum.GetValues(typeof(Direction)))
      {
        if (line.StartsWith(direction.ToString()))
        {
          line = line.Substring(direction.ToString().Length);
          return direction;
        }
      }
      throw new Exception("💥");
    }
  }
}
