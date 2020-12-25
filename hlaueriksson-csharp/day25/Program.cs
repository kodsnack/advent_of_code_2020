using System;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");
var card = new Device(lines.First());
var door = new Device(lines.Last());

Console.WriteLine(PartOne());
Console.OutputEncoding = System.Text.Encoding.UTF8;
Console.WriteLine(PartTwo());

long PartOne()
{
  card.LoopSize = card.BruteForceLoopSize();
  door.LoopSize = door.BruteForceLoopSize();

  return card.Transform(door.PublicKey);
}

string PartTwo() => "⭐";

class Device
{
  public long PublicKey { get; set; }
  public int LoopSize { get; set; }

  public Device(string line)
  {
    PublicKey = Convert.ToInt64(line);
  }

  public long Transform(long subjectNumber)
  {
    long value = 1;
    for (int i = 0; i < LoopSize; i++)
    {
      value = value * subjectNumber;
      value = value % 20201227;
    }
    return value;
  }

  public int BruteForceLoopSize()
  {
    var result = 0;
    long subjectNumber = 7;
    long value = 1;
    while (value != PublicKey)
    {
      result++;
      value = value * subjectNumber;
      value = value % 20201227;
    }
    return result;
  }
}
