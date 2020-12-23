using System;
using System.Collections.Generic;
using System.Linq;

var isLogEnabled = false;
var input = "418976235";

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

string PartOne()
{
  var circle = new Circle(input);

  for (var move = 1; move <= 100; move++)
  {
    Log($"-- move {move} --");
    Log($"cups: {circle}");
    var picks = circle.PickUp();
    Log($"pick up: {string.Join(", ", picks)}");
    var destination = circle.DestinationCup();
    Log($"destination: {destination}");
    circle.Place(picks, destination);
    circle.UpdateCurrentCup();
    Log();
  }
  Log("-- final --");
  Log($"cups: {circle}");

  return circle.GetResult();
}

string PartTwo()
{
  var circle = new BigCircle(input, 1_000_000);

  for (var move = 1; move <= 10_000_000; move++)
  {
    var picks = circle.PickUp();
    var destination = circle.DestinationCup();
    circle.Place(picks, destination);
    circle.UpdateCurrentCup();
  }

  return circle.GetResult();
}

void Log(string message = null)
{
  if (isLogEnabled) Console.WriteLine(message);
}

interface ICircle
{
  IEnumerable<int> PickUp();
  int DestinationCup();
  void Place(IEnumerable<int> pickedUpCups, int destinationCup);
  void UpdateCurrentCup();
}

class Circle : ICircle
{
  List<int> Cups { get; }
  int CurrentCup { get; set; }
  int CurrentCupIndex => GetCupIndex(CurrentCup);

  public Circle(string input)
  {
    Cups = input.Select(x => x - '0').ToList();
    CurrentCup = Cups[0];
  }

  public IEnumerable<int> PickUp()
  {
    var indexes = Enumerable.Range(CurrentCupIndex + 1, 3).Select(GetIndex);
    var cups = indexes.Select(i => Cups[i]).ToArray();
    foreach (var cup in cups) Cups.Remove(cup);
    return cups;
  }

  public int DestinationCup()
  {
    var min = Cups.Min();
    var max = Cups.Max();
    var result = CurrentCup - 1;
    while (!Cups.Contains(result))
    {
      result--;
      if (result < min) result = max;
    }
    return result;
  }

  public void Place(IEnumerable<int> pickedUpCups, int destinationCup)
  {
    var index = Cups.IndexOf(destinationCup) + 1;
    Cups.InsertRange(index, pickedUpCups);
  }

  public void UpdateCurrentCup()
  {
    CurrentCup = Cups[GetIndex(CurrentCupIndex + 1)];
  }

  public string GetResult()
  {
    var indexes = Enumerable.Range(GetCupIndex(1) + 1, Cups.Count - 1).Select(GetIndex);
    return string.Join(null, indexes.Select(i => Cups[i]));
  }

  public override string ToString()
  {
    return string.Join(" ", Enumerable.Range(0, Cups.Count).Select(CupAsString));

    string CupAsString(int index) => index == CurrentCupIndex ? $"({Cups[index]})" : $"{Cups[index]} ";
  }

  int GetCupIndex(int cup) => Cups.IndexOf(cup);

  int GetIndex(int index) => index % Cups.Count;
}

class BigCircle : ICircle
{
  class Cup
  {
    public int Value { get; set; }
    public Cup Next { get; set; }

    public Cup(int value)
    {
      Value = value;
    }
  }

  int Count { get; }
  Dictionary<int, Cup> Cups { get; } = new Dictionary<int, Cup>();
  Cup CurrentCup { get; set; }
  List<int> PickedUpCups { get; } = new List<int>();

  public BigCircle(string input, int count)
  {
    Count = count;
    var numbers = input.Select(x => x - '0').ToArray();
    CurrentCup = new Cup(numbers.First());
    Cups.Add(CurrentCup.Value, CurrentCup);
    var previous = CurrentCup;
    for (var i = 1; i < numbers.Length; i++)
    {
      var cup = new Cup(numbers[i]);
      Cups.Add(cup.Value, cup);
      previous.Next = cup;
      previous = cup;
    }
    for (var i = numbers.Max() + 1; i <= Count; i++)
    {
      var cup = new Cup(i);
      Cups.Add(cup.Value, cup);
      previous.Next = cup;
      previous = cup;
    }
    previous.Next = CurrentCup;
  }

  public IEnumerable<int> PickUp()
  {
    PickedUpCups.Clear();
    var cup = CurrentCup;
    for (var i = 0; i < 3; i++)
    {
      cup = cup.Next;
      PickedUpCups.Add(cup.Value);
    }
    CurrentCup.Next = cup.Next;
    return PickedUpCups;
  }

  public int DestinationCup()
  {
    var result = CurrentCup.Value - 1;
    if (result == 0) result = Count;
    while (PickedUpCups.Any(x => x == result))
    {
      result--;
      if (result == 0) result = Count;
    }
    return result;
  }

  public void Place(IEnumerable<int> pickedUpCups, int destinationCup)
  {
    var cup = Cups[destinationCup];
    var temp = cup.Next;
    cup.Next = Cups[pickedUpCups.First()];
    Cups[pickedUpCups.Last()].Next = temp;
  }

  public void UpdateCurrentCup()
  {
    CurrentCup = CurrentCup.Next;
  }

  public string GetResult()
  {
    long result = (long)Cups[1].Next.Value * (long)Cups[1].Next.Next.Value;
    return result.ToString();
  }
}
