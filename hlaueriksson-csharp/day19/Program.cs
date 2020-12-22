using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var rules = new Dictionary<string, Rule>();
  var messages = new List<string>();

  foreach (var line in lines)
  {
    if (line == string.Empty) continue;

    if (line.Contains(":"))
    {
      var tokens = line.Split(":");
      rules.Add(tokens.First(), new Rule { Text = tokens.Last().Trim() });
    }
    else
    {
      messages.Add(line);
    }
  }

  var pattern = ExpandRule(rules["0"]);
  var regex = new Regex("^" + pattern + "$", RegexOptions.Compiled);

  return messages.Where(x => regex.IsMatch(x)).Count();

  string ExpandRule(Rule rule)
  {
    if (rule.Text.StartsWith("\""))
      return rule.Text.Substring(1, 1);

    var expanded = rule.Text.Split(" ").Aggregate("", (result, value) => result + (value == "|" ? "|" : ExpandRule(rules[value])));

    return $"({expanded})";
  }
}

int PartTwo()
{
  var rules = new Dictionary<string, Rule>();
  var messages = new List<string>();

  foreach (var line in lines)
  {
    if (line == string.Empty) continue;

    if (line.Contains(":"))
    {
      var tokens = line.Split(":");
      rules.Add(tokens.First(), new Rule { Id = tokens.First(), Text = tokens.Last().Trim() });
    }
    else
    {
      messages.Add(line);
    }
  }

  rules["8"].Text = "42 | 42 8";
  rules["11"].Text = "42 31 | 42 11 31";

  var magicCount = 5; // flatlines here
  var pattern = ExpandRule(rules["0"], 0, 0);
  var regex = new Regex("^" + pattern + "$", RegexOptions.Compiled);

  return messages.Where(x => regex.IsMatch(x)).Count();

  string ExpandRule(Rule rule, int count8, int count11)
  {
    if (rule.Text.StartsWith("\""))
      return rule.Text.Substring(1, 1);

    if (rule.Id == "8") count8++;
    if (rule.Id == "11") count11++;

    if (rule.Id == "8" && count8 == magicCount)
      rules["8"].Text = "42";
    if (rule.Id == "11" && count11 == magicCount)
      rules["11"].Text = "42 31";

    var expanded = rule.Text.Split(" ").Aggregate("", (result, value) => result + (value == "|" ? "|" : ExpandRule(rules[value], count8, count11)));

    return $"({expanded})";
  }
}

class Rule
{
  public string Id { get; set; }
  public string Text { get; set; }
}
