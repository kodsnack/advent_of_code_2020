using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using AdventOfCode;

namespace day03
{
    public class Day03
    {
        readonly static string nsname = typeof(Day03).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 03: Toboggan Trajectory - Check occurrences in 2D character map

        static Object PartA()
        {
            List<string> input = ReadIndata.Strings(inputPath);
            int ans = 0;
            int c = 0;
            for (int r = 0; r < input.Count; r++)
            {
                if (input[r][c % input[0].Count()] == '#')
                    ans++;
                c += 3;
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static int CalcTrees(List<string> input, int radd, int cadd)
        {
            int a = 0;
            int c = 0;
            for (int r = 0; r < input.Count; r += radd)
            {
                if (input[r][c % input[0].Count()] == '#')
                    a++;
                c += cadd;
            }
            return a;
        }

        static Object PartB()
        {
            List<string> input = ReadIndata.Strings(inputPath);
            long ans = CalcTrees(input, 1, 1);
            ans *= CalcTrees(input, 1, 3);
            ans *= CalcTrees(input, 1, 5);
            ans *= CalcTrees(input, 1, 7);
            ans *= CalcTrees(input, 2, 1);
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
            int a = 218;
            long b = 3847183340;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
