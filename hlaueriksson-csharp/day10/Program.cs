using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");
var numbers = lines.Select(x => Convert.ToInt32(x)).ToList();
numbers.Add(0);
numbers.Add(numbers.Max() + 3);
numbers.Sort();

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  int one = 0, three = 0;

  for (var i = 0; i < numbers.Count - 1; i++)
  {
    var difference = numbers.ElementAt(i + 1) - numbers.ElementAt(i);

    switch (difference)
    {
      case 1:
        one++;
        break;
      case 3:
        three++;
        break;
    }
  }

  return one * three;
}

long PartTwo()
{
  var arrangements = new Dictionary<int, long>();
  var range = Enumerable.Range(1, 3).ToArray();

  foreach (var number in numbers)
  {
    long count = 0;
    if (number == 0)
    {
      count = 1;
    }
    else
    {
      foreach (var x in range)
      {
        count += arrangements.GetValueOrDefault(number - x);
      }
    }
    arrangements[number] = count;
  }

  return arrangements.Last().Value;
}
