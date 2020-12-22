using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

var lines = File.ReadAllLines("input.txt").ToList();

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var rules = GetRules();
  var tickets = GetTickets();
  var errors = new List<int>();

  foreach (var number in tickets.SelectMany(x => x))
  {
    if (!IsValidNumber(rules, number)) errors.Add(number);
  }

  return errors.Sum();
}

long PartTwo()
{
  var rules = GetRules();
  var myTicket = GetMyTicket();
  var tickets = GetTickets();
  var validTickets = GetValidTickets();
  CalculateValidFieldOrder();
  var values = GetDepartureFieldValues();

  return values.Aggregate((a, b) => a * b);

  List<int> GetMyTicket()
  {
    var index = lines.IndexOf("your ticket:");
    return lines[index + 1].Split(",").Select(int.Parse).ToList();
  }

  List<List<int>> GetValidTickets()
  {
    var result = new List<List<int>>();

    foreach (var ticket in tickets)
    {
      if (IsValidTicket(rules, ticket)) result.Add(ticket.ToList());
    }

    return result;
  }

  void CalculateValidFieldOrder()
  {
    var rulesToFields = new Dictionary<Rule, HashSet<int>>();

    foreach (var rule in rules)
    {
      var fields = Enumerable.Range(0, rules.Count).ToHashSet();
      for (int i = 0; i < rules.Count; i++)
      {
        foreach (var ticket in validTickets)
        {
          if (!IsValid(rule, ticket[i]))
          {
            fields.Remove(i);
            break;
          }
        }
      }

      rulesToFields[rule] = fields;
    }

    while (rulesToFields.Any(x => x.Value.Count > 0))
    {
      var ready = rulesToFields.First(x => x.Value.Count == 1);
      var index = ready.Value.Single();
      ready.Key.Index = index;

      foreach (var kvp in rulesToFields)
      {
        kvp.Value.Remove(index);
      }
    }
  }

  IEnumerable<long> GetDepartureFieldValues()
  {
    var indexes = rules.Where(x => x.Field.StartsWith("departure")).Select(x => x.Index);

    foreach (var index in indexes)
    {
      yield return myTicket[index];
    }
  }
}

List<Rule> GetRules()
{
  var result = new List<Rule>();
  var regex = new Regex(@"^(.*): (\d+)-(\d+) or (\d+)-(\d+)$");

  foreach (var line in lines)
  {
    if (line == string.Empty) break;

    var match = regex.Match(line);
    var rule = new Rule(match.Groups[1].Value, new[]
    {
      new Range(GetValue(match, 2), GetValue(match, 3)),
      new Range(GetValue(match, 4), GetValue(match, 5))
    });
    result.Add(rule);
  }

  return result;

  int GetValue(Match match, int index) => Convert.ToInt32(match.Groups[index].Value);
}

IEnumerable<IEnumerable<int>> GetTickets()
{
  var tickets = new List<int>();
  var index = lines.IndexOf("nearby tickets:");

  for (int i = index + 1; i < lines.Count; i++)
  {
    var numbers = lines[i].Split(",").Select(int.Parse);
    yield return numbers;
  }
}

bool IsValidTicket(List<Rule> rules, IEnumerable<int> ticket) =>
  ticket.All(number => IsValidNumber(rules, number));

bool IsValidNumber(List<Rule> rules, int number) =>
  rules.Any(rule => IsValid(rule, number));

bool IsValid(Rule rule, int number) =>
  (number >= rule.Ranges[0].From && number <= rule.Ranges[0].To) ||
  (number >= rule.Ranges[1].From && number <= rule.Ranges[1].To);

record Rule(string Field, Range[] Ranges)
{
  public int Index { get; set; }
}

record Range(int From, int To);
