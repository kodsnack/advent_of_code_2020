using System;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne() => lines.Select(GetSeatId).ToList().Max();

int PartTwo()
{
  var seats = lines.Select(GetSeatId).ToList();
  var range = Enumerable.Range(seats.Min(), seats.Count()).ToList();
  var result = range.Except(seats);
  return result.Single();
}

int GetSeatId(string line) => GetRow(line) * 8 + GetColumn(line);

int GetRow(string line)
{
  int min = 0, max = 127;
  for (int i = 0; i < 7; i++)
  {
    switch (line[i])
    {
      case 'F':
        max = (max + min) / 2;
        break;
      case 'B':
        min = (max + min) / 2 + 1;
        break;
    }
  }
  return min;
}

int GetColumn(string line)
{
  int min = 0, max = 7;
  for (int i = 7; i < 10; i++)
  {
    switch (line[i])
    {
      case 'L':
        max = (max + min) / 2;
        break;
      case 'R':
        min = (max + min) / 2 + 1;
        break;
    }
  }
  return min;
}
