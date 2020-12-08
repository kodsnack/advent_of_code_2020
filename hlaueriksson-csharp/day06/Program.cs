using System;
using System.IO;
using System.Linq;

var text = File.ReadAllText("input.txt").Trim();
var groups = text.Split("\n\n");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne() => groups.Select(GetDistinctAnswerCount).Sum();

int PartTwo() => groups.Select(GetAllAnswerCount).Sum();

int GetDistinctAnswerCount(string group) =>
  group.Replace("\n", string.Empty).ToCharArray().Distinct().Count();

int GetAllAnswerCount(string group)
{
  var personCount = group.Count(x => x == '\n') + 1;
  var answers = group.Replace("\n", string.Empty).ToCharArray();
  var answerGroups = answers.GroupBy(x => x);
  return answerGroups.Count(x => x.Count() == personCount);
}
