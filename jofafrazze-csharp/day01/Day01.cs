using System;
using System.Collections.Generic;
using System.IO;

using AdventOfCode;

namespace day01
{
    public class Day01
    {
        readonly static string nsname = typeof(Day01).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static Object PartA()
        {
            List<int> input = ReadInputs.ReadInts(inputPath);
            input.Sort();
            int ans = 0;
            foreach (int a in input)
            {
                int b = 2020 - a;
                if (input.Contains(b))
                {
                    ans = a * b;
                    break;
                }
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            List<long> input = ReadInputs.ReadLongs(inputPath);
            input.Sort();
            long ans = 0;
            for (int i = 0; i < input.Count; i++)
            {
                for (int j = 0; j < input.Count; j++)
                {
                    if (j != i)
                    {
                        long a = input[i];
                        long b = input[j];
                        long c = 2020 - a - b;
                        if (input.Contains(c))
                        {
                            ans = a * b * c;
                            break;
                        }
                    }
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
            int a = 800139; 
            long b = 59885340;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
