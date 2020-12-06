using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day05
{
    public class Day05
    {
        readonly static string nsname = typeof(Day05).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static HashSet<int> GetIds(List<string> list)
        {
            HashSet<int> ids = new HashSet<int>();
            foreach (string s in list)
            {
                int row = 0;
                for (int i = 0; i < 7; i++)
                {
                    row <<= 1;
                    int bit = s[i] == 'F' ? 0 : 1;
                    row |= bit;
                }
                int col = 0;
                for (int i = 7; i < 10; i++)
                {
                    col <<= 1;
                    int bit = s[i] == 'L' ? 0 : 1;
                    col |= bit;
                }
                ids.Add(row * 8 + col);
            }
            return ids;
        }

        static Object PartA()
        {
            List<string> input = ReadInputs.ReadStrings(inputPath);
            HashSet<int> ids = GetIds(input);
            int ans = ids.Max();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            List<string> input = ReadInputs.ReadStrings(inputPath);
            HashSet<int> ids = GetIds(input);
            int ans = ids.Where(x => !ids.Contains(x + 1) && ids.Contains(x + 2)).Min() + 1;
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
            int a = 906;
            int b = 519;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
