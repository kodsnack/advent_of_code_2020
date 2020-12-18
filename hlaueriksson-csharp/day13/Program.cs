using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");
var timestamp = Convert.ToInt32(lines.First());
var busses = lines.Last().Split(",").Select((Id, Index) => new { Id, Index }).Where(x => x.Id != "x").Select(x => new { Id = Convert.ToInt64(x.Id), x.Index }).ToArray();

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

long PartOne()
{
  var result = new Dictionary<long, long>();

  foreach (var bus in busses)
  {
    long time = 0;
    while (time < timestamp)
    {
      time += bus.Id;
    }
    result[bus.Id] = time;
  }

  var first = result.OrderBy(x => x.Value).First();
  return first.Key * (first.Value - timestamp);
}

long PartTwo()
{
  var steps = busses.First().Id;
  long time = 0;

  foreach (var bus in busses.Skip(1))
  {
    while (true)
    {
      if ((time + bus.Index) % bus.Id == 0)
      {
        steps *= bus.Id;
        break;
      }
      time += steps;
    }
  }
  return time;
}
