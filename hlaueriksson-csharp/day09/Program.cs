using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");
var numbers = lines.Select(x => Convert.ToInt64(x)).ToArray();
int preamble = 25, consider = 25;

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

long PartOne() => GetInvalidNumber();

long PartTwo() => FindWeakness(GetInvalidNumber());

long GetInvalidNumber()
{
  for (var i = preamble; i < numbers.Length; i++)
  {
    var number = numbers[i];

    if (!IsValid(number, numbers.Skip(i - consider).Take(consider).ToArray())) return number;
  }

  return default(long);
}

bool IsValid(long number, long[] previous)
{
  for (var i = 0; i < previous.Length - 1; i++)
  {
    for (var j = i + 1; j < previous.Length; j++)
    {
      if (previous[i] + previous[j] == number) return true;
    }
  }

  return false;
}

long FindWeakness(long number)
{
  for (var i = 0; i < numbers.Length - 1; i++)
  {
    var index = i;
    var range = new List<long> { numbers[index] };

    while (range.Sum() < number)
    {
      range.Add(numbers[++index]);
    }

    if (range.Sum() == number) return range.Min() + range.Max();
  }

  return default(long);
}
