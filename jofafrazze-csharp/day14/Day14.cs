using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day14
{
    public class Day14
    {
        readonly static string nsname = typeof(Day14).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 14: Docking Data - Writing memory using bitmask/logic applied to data then address

        const int bits = 36;

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            string mask = "";
            var mem = new Dictionary<int, long>();
            foreach (var s in input)
            {
                var t = s.Split("[] =".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                if (t.Contains("mask"))
                {
                    mask = t[1];
                }
                else
                {
                    var a = Convert.ToString(long.Parse(t[2]), 2).PadLeft(bits, '0');
                    var b = "";
                    for (int i = 0; i < bits; i++)
                        b += mask[i] == 'X' ? a[i] : mask[i];
                    mem[int.Parse(t[1])] = Convert.ToInt64(b, 2);
                }
            }
            long ans = mem.Select(x => x.Value).Sum();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            string mask = "";
            var mem = new Dictionary<long, long>();
            foreach (var s in input)
            {
                var t = s.Split("[] =".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                if (t.Contains("mask"))
                {
                    mask = t[1];
                }
                else
                {
                    var a = Convert.ToString(long.Parse(t[1]), 2).PadLeft(bits, '0');
                    var b = "";
                    var offs = new List<long>() { 0 };
                    for (int i = 0; i < bits; i++)
                    {
                        var m = mask[i];
                        if (m == 'X')
                            offs.Add(1L << bits - 1 - i);
                        b += m == 'X' ? '0' : (m == '1' ? '1' : a[i]);
                    }
                    var membase = Convert.ToInt64(b, 2);
                    var value = long.Parse(t[2]);
                    foreach (var z in Algorithms.GetCombinations(offs))
                        mem[membase + z.Sum()] = value;
                }
            }
            long ans = mem.Select(x => x.Value).Sum();
            Console.WriteLine("Part B: Result is {0}", ans);
            return ans;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2020 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            long a = 10452688630537;
            long b = 2881082759597;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
