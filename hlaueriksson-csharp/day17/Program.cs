using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var cubes = new Dictionary<(int, int, int), char>();
  for (int y = 0; y < lines.Length; y++)
    for (int x = 0; x < lines[y].Length; x++)
      cubes[(0, y, x)] = lines[y][x];

  for (int i = 0; i < 6; i++)
  {
    var next = Run(cubes);
    Copy(next, cubes);
  }

  return cubes.Where(x => x.Value == '#').Count();

  Dictionary<(int z, int y, int x), char> Run(Dictionary<(int z, int y, int x), char> cubes)
  {
    var next = new Dictionary<(int z, int y, int x), char>();
    Copy(cubes, next);

    foreach (var key in next.Keys)
    {
      AddNeighborsToCubes(GetNeighbors(key.z, key.y, key.x));
    }

    foreach (var key in cubes.Keys)
    {
      Flip(key.z, key.y, key.x);
    }

    return next;

    void Flip(int z, int y, int x)
    {
      var neighbors = GetNeighbors(z, y, x);
      var activeNeighborsCount = neighbors.Count(n => IsActive(n.z, n.y, n.x));

      if (IsActive(z, y, x) && activeNeighborsCount != 2 && activeNeighborsCount != 3)
      {
        next[(z, y, x)] = '.';
      }
      else if (IsInactive(z, y, x) && activeNeighborsCount == 3)
      {
        next[(z, y, x)] = '#';
      }
    }

    (int z, int y, int x)[] GetNeighbors(int z, int y, int x)
    {
      return new (int, int, int)[] // 26
      {
        new (z-1, y-1, x-1), new (z-1, y-1, x), new (z-1, y-1, x+1),
        new (z-1, y,   x-1), new (z-1, y,   x), new (z-1, y,   x+1),
        new (z-1, y+1, x-1), new (z-1, y+1, x), new (z-1, y+1, x+1),

        new (z,   y-1, x-1), new (z,   y-1, x), new (z,   y-1, x+1),
        new (z,   y,   x-1),                    new (z,   y,   x+1),
        new (z,   y+1, x-1), new (z,   y+1, x), new (z,   y+1, x+1),

        new (z+1, y-1, x-1), new (z+1, y-1, x), new (z+1, y-1, x+1),
        new (z+1, y,   x-1), new (z+1, y,   x), new (z+1, y,   x+1),
        new (z+1, y+1, x-1), new (z+1, y+1, x), new (z+1, y+1, x+1),
      };
    }

    void AddNeighborsToCubes((int z, int y, int x)[] neighbors)
    {
      foreach (var n in neighbors)
      {
        if (!cubes.ContainsKey((n.z, n.y, n.x)))
        {
          cubes[(n.z, n.y, n.x)] = '.';
        }
      }
    }

    bool IsInactive(int z, int y, int x) => cubes[(z, y, x)] == '.';

    bool IsActive(int z, int y, int x)
    {
      if (!cubes.ContainsKey((z, y, x))) return false;
      return cubes[(z, y, x)] == '#';
    }
  }
}

int PartTwo()
{
  var cubes = new Dictionary<(int, int, int, int), char>();
  for (int y = 0; y < lines.Length; y++)
    for (int x = 0; x < lines[y].Length; x++)
      cubes[(0, 0, y, x)] = lines[y][x];

  for (int i = 0; i < 6; i++)
  {
    var next = Run(cubes);
    Copy(next, cubes);
  }

  return cubes.Where(x => x.Value == '#').Count();

  Dictionary<(int w, int z, int y, int x), char> Run(Dictionary<(int w, int z, int y, int x), char> cubes)
  {
    var next = new Dictionary<(int w, int z, int y, int x), char>();
    Copy(cubes, next);

    foreach (var key in next.Keys)
    {
      AddNeighborsToCubes(GetNeighbors(key.w, key.z, key.y, key.x));
    }

    foreach (var key in cubes.Keys)
    {
      Flip(key.w, key.z, key.y, key.x);
    }

    return next;

    void Flip(int w, int z, int y, int x)
    {
      var neighbors = GetNeighbors(w, z, y, x);
      var activeNeighborsCount = neighbors.Count(n => IsActive(n.w, n.z, n.y, n.x));

      if (IsActive(w, z, y, x) && activeNeighborsCount != 2 && activeNeighborsCount != 3)
      {
        next[(w, z, y, x)] = '.';
      }
      else if (IsInactive(w, z, y, x) && activeNeighborsCount == 3)
      {
        next[(w, z, y, x)] = '#';
      }
    }

    (int w, int z, int y, int x)[] GetNeighbors(int w, int z, int y, int x)
    {
      var result = new List<(int, int, int, int)>();
      var offsets = new int[] { -1, 0, 1 };

      foreach (var deltaW in offsets)
        foreach (var deltaZ in offsets)
          foreach (var deltaY in offsets)
            foreach (var deltaX in offsets)
            {
              if (deltaW == 0 && deltaZ == 0 && deltaY == 0 && deltaX == 0) continue;
              result.Add((w + deltaW, z + deltaZ, y + deltaY, x + deltaX));
            }

      return result.ToArray();
    }

    void AddNeighborsToCubes((int w, int z, int y, int x)[] neighbors)
    {
      foreach (var n in neighbors)
      {
        if (!cubes.ContainsKey((n.w, n.z, n.y, n.x)))
        {
          cubes[(n.w, n.z, n.y, n.x)] = '.';
        }
      }
    }

    bool IsInactive(int w, int z, int y, int x) => cubes[(w, z, y, x)] == '.';

    bool IsActive(int w, int z, int y, int x)
    {
      if (!cubes.ContainsKey((w, z, y, x))) return false;
      return cubes[(w, z, y, x)] == '#';
    }
  }
}

void Copy(IDictionary from, IDictionary to)
{
  foreach (var key in from.Keys)
    to[key] = from[key];
}
