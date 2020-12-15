using System;
using System.Collections.Generic;
using System.IO;
using AdventOfCode;

namespace day15
{
    public class Day15
    {
        readonly static string nsname = typeof(Day15).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static int MemoryGame(List<int> input, int iters)
        {
            int spoken = 0;
            var d = new Dictionary<int, List<int>>();
            bool AddNum(int n, int turn)
            {
                spoken = n;
                bool exist = d.ContainsKey(n);
                if (!exist)
                    d[n] = new List<int>();
                d[n].Add(turn);
                return !exist;
            }
            bool first = false;
            for (int i = 1; i <= iters; i++)
            {
                if (i <= input.Count)
                    first = AddNum(input[i - 1], i);
                else if (first)
                    first = AddNum(0, i);
                else
                {
                    int c = d[spoken].Count;
                    first = AddNum(d[spoken][c - 1] - d[spoken][c - 2], i);
                }
            }
            return spoken;
        }

        static Object PartA()
        {
            var input = ReadIndata.Ints(inputPath);
            int ans = MemoryGame(input, 2020);
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadIndata.Ints(inputPath);
            int ans = MemoryGame(input, 30000000);
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
            int a = 981;
            int b = 164878;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
