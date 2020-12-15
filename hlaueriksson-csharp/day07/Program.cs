using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

var lines = File.ReadAllLines("input.txt");

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne() => GetBags().Where(x => x.Contains("shiny gold")).Count();

int PartTwo() => GetBags().Single(x => x.Color == "shiny gold").GetChildCount();

List<Bag> GetBags()
{
  var result = new List<Bag>();

  foreach (var line in lines)
  {
    var tokens = line.Split("bags contain");
    var color = tokens.First().Trim();
    var children = tokens.Last().Split(",").Where(x => !x.Contains("no other bags")).Select(x => GetCountAndColor(x));

    var bag = GetOrCreateBag(color);

    foreach (var child in children)
    {
      bag.Children[GetOrCreateBag(child.color)] = child.count;
    }
  }

  return result;

  (int count, string color) GetCountAndColor(string text)
  {
    var regex = new Regex("^([1-9]+) (.*) bag.*$");
    var matches = regex.Matches(text.Trim());
    var groups = matches.First().Groups;
    return (Convert.ToInt32(groups[1].Value), groups[2].Value);
  }

  Bag GetOrCreateBag(string color)
  {
    var bag = result.SingleOrDefault(x => x.Color == color);
    if (bag != null) return bag;

    bag = new Bag(color);
    result.Add(bag);
    return bag;
  }
}

class Bag
{
  public string Color { get; }
  public Dictionary<Bag, int> Children = new Dictionary<Bag, int>();

  public Bag(string color)
  {
    Color = color;
  }

  public bool Contains(string color)
  {
    return Children.Where(x => x.Key.Color == color || x.Key.Contains(color)).Count() > 0;
  }

  internal int GetChildCount()
  {
    return Children.Select(x => x.Value * (x.Key.GetChildCount() + 1)).Sum();
  }
}
