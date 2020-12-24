using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

var lines = File.ReadAllLines("input.txt");
var regex = new Regex(@"^mem\[(\d+)\] = (\d+)$", RegexOptions.Compiled);

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

long PartOne()
{
  var mask = string.Empty;
  var mem = new Dictionary<long, long>();

  foreach (var line in lines)
  {
    if (line.StartsWith("mask"))
    {
      mask = line.Substring("mask = ".Length);
    }
    else
    {
      var match = regex.Match(line);
      var address = Convert.ToInt32(match.Groups[1].Value);
      var value = Convert.ToInt64(match.Groups[2].Value);

      mem[address] = ApplyMask(value);
    }
  }

  return mem.Sum(x => x.Value);

  long ApplyMask(long value)
  {
    var bits = new StringBuilder(Convert.ToString(value, 2).PadLeft(36, '0'));

    for (int i = 0; i < mask.Count(); i++)
    {
      if (mask[i] == 'X') continue;

      bits[i] = mask[i];
    }

    return Convert.ToInt64(bits.ToString(), 2);
  }
}

long PartTwo()
{
  var mask = string.Empty;
  var mem = new Dictionary<long, long>();

  foreach (var line in lines)
  {
    if (line.StartsWith("mask"))
    {
      mask = line.Substring("mask = ".Length);
    }
    else
    {
      var match = regex.Match(line);
      var address = Convert.ToInt32(match.Groups[1].Value);
      var value = Convert.ToInt64(match.Groups[2].Value);
      var addresses = GetAddresses(address);

      foreach (var a in addresses)
      {
        mem[a] = value;
      }
    }
  }

  return mem.Sum(x => x.Value);

  IEnumerable<long> GetAddresses(long address)
  {
    var bits = Convert.ToString(address, 2).PadLeft(36, '0');
    var length = mask.Count(x => x == 'X');
    var permutations = GetPermutationsWithRept<char>(new[] { '0', '1' }, length);

    foreach (var permutation in permutations)
    {
      var result = new StringBuilder(bits);
      var x = 0;
      for (int i = 0; i < mask.Count(); i++)
      {
        if (mask[i] == '0') continue;

        if (mask[i] == '1')
        {
          result[i] = '1';
        }
        else
        {
          result[i] = permutation.ElementAt(x);
          x++;
        }
      }

      yield return Convert.ToInt64(result.ToString(), 2);
    }

    IEnumerable<IEnumerable<T>> GetPermutationsWithRept<T>(IEnumerable<T> list, int length)
    {
      if (length == 1) return list.Select(t => new T[] { t });
      return GetPermutationsWithRept(list, length - 1).SelectMany(t => list, (t1, t2) => t1.Concat(new T[] { t2 }));
    }
  }
}
