using System;
using System.IO;
using System.Linq;

var numbers = File.ReadLines("input.txt").Select(x => Convert.ToInt32(x)).ToList();
Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  foreach (var firstNumber in numbers)
  {
    var secondNumber = numbers.SingleOrDefault(x => firstNumber + x is 2020);

    if (secondNumber is not default(int))
    {
      return firstNumber * secondNumber;
    }
  }
  return default(int);
}

int PartTwo()
{
  foreach (var firstNumber in numbers)
  {
    foreach (var secondNumber in numbers)
    {
      var thirdNumber = numbers.SingleOrDefault(x => firstNumber + secondNumber + x is 2020);

      if (thirdNumber is not default(int))
      {
        return firstNumber * secondNumber * thirdNumber;
      }
    }
  }
  return default(int);
}
