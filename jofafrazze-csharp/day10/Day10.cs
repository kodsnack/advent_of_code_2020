using System;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day10
{
    public class Day10
    {
        readonly static string nsname = typeof(Day10).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 10: Adapter Array - Order numbers, deltas, find possible configurations (Tribonacci)

        static Object PartA()
        {
            var input = ReadIndata.Ints(inputPath);
            input.Add(0);
            input.Add(input.Max() + 3);
            input.Sort();
            var d = new int[] { 0, 0, 0, 0 };
            for (int i = 1; i < input.Count; i++)
                d[input[i] - input[i - 1]]++;
            int ans = d[1] * d[3];
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadIndata.Ints(inputPath);
            input.Add(0);
            input.Add(input.Max() + 3);
            input.Sort();
            long ans = 1;
            int i = 0;
            int z = input.Count;
            var combs = new int[] { 0, 1, 1, 2, 4, 7 };
            while (i < z - 1)
            {
                int n = 1;
                while ((i + n < z) && (input[i + n] - input[i + n - 1] == 1))
                    n++;
                ans *= combs[n];
                i += n;
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
            int a = 1700;
            long b = 12401793332096;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
