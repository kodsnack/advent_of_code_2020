using System;
using System.Collections.Generic;
using System.Linq;

var input = "18,11,9,0,5,1";
var start = input.Split(",").Select(int.Parse).ToList();

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var numbers = new List<int>(start);

  for (int i = start.Count; i < 2020; i++)
  {
    var last = numbers[i - 1];
    if (numbers.Count(x => x == last) == 1)
    {
      numbers.Add(0);
    }
    else
    {
      var index = numbers.LastIndexOf(last, numbers.Count - 2);

      numbers.Add(numbers.Count - (index + 1));
    }
  }

  return numbers.Last();
}

int PartTwo()
{
  var count = 30000000;
  var numbers = new Dictionary<int, List<int>>(count);

  for (int i = 0; i < start.Count; i++)
  {
    AddOrUpdate(start[i], i);
  }

  var last = start.Last();

  for (int i = start.Count; i < count; i++)
  {
    var next = 0;
    if (numbers[last].Count > 1)
    {
      next = i - (numbers[last][^2] + 1);
    }

    AddOrUpdate(next, i);
    last = next;
  }

  return last;

  void AddOrUpdate(int number, int index)
  {
    if (numbers.ContainsKey(number))
    {
      numbers[number].Add(index);
    }
    else
    {
      numbers[number] = new List<int> { index };
    }
  }
}
