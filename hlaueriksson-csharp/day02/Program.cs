using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

var lines = File.ReadAllLines("input.txt");
var regex = new Regex(@"^(\d{1,2})-(\d{1,2}) ([a-zA-Z]): ([a-zA-Z]*)$", RegexOptions.Compiled);

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var validCount = 0;

  foreach (var line in lines)
  {
    var (min, max, letter, password) = GetLineData(line);
    var count = password.Count(x => x == letter);
    var isValid = count >= min && count <= max;

    if (isValid) validCount++;
  }
  return validCount;
}

int PartTwo()
{
  var validCount = 0;

  foreach (var line in lines)
  {
    var (firstPosition, secondPosition, letter, password) = GetLineData(line);
    var firstLetter = password.ElementAt(firstPosition - 1);
    var secondLetter = password.ElementAt(secondPosition - 1);
    var isValid = firstLetter == letter ^ secondLetter == letter;

    if (isValid) validCount++;
  }
  return validCount;
}

(int firstNumber, int secondNumber, char letter, string password) GetLineData(string line)
{
  var matches = regex.Matches(line);
  var groups = matches.First().Groups;

  return (
    Convert.ToInt32(groups[1].Value),
    Convert.ToInt32(groups[2].Value),
    groups[3].Value.ToCharArray().Single(),
    groups[4].Value
  );
}
