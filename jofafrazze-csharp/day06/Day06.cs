using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using AdventOfCode;

namespace day06
{
    public class Day06
    {
        readonly static string nsname = typeof(Day06).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 06: Custom Customs - Parse groups and questions, perform some and/or logic

        static Object PartA()
        {
            List<string> input = ReadIndata.Strings(inputPath);
            int ans = 0;
            HashSet<char> set = new HashSet<char>();
            foreach (string s in input)
            {
                if (s == "")
                {
                    ans += set.Count;
                    set = new HashSet<char>();
                }
                else
                    set.UnionWith(s.ToHashSet());
            }
            ans += set.Count;
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            List<string> input = ReadIndata.Strings(inputPath);
            int ans = 0;
            Dictionary<char, int> dict = new Dictionary<char, int>();
            int n = 0;
            foreach (string s in input)
            {
                if (s == "")
                {
                    ans += dict.Where(x => x.Value == n).Count();
                    dict = new Dictionary<char, int>();
                    n = 0;
                }
                else
                {
                    foreach (char c in s)
                    {
                        if (!dict.ContainsKey(c))
                            dict[c] = 0;
                        dict[c]++;
                    }
                    n++;
                }
            }
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
            int a = 6521;
            int b = 3305;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
