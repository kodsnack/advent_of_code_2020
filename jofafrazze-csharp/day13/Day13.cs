using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day13
{
    public class Day13
    {
        readonly static string nsname = typeof(Day13).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            List<int> ids = input[1].Split(',').Where(x => x != "x").Select(x => int.Parse(x)).ToList();
            int minutes = int.Parse(input[0]);
            int min = int.MaxValue;
            int ans = 0;
            foreach (int i in ids)
            {
                int a = i - (minutes % i);
                if (a < min)
                {
                    min = a;
                    ans = a * i;
                }
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static long ChineseRemainder(List<int> num, List<int> rem)
        {
            long prod = num.Aggregate((long)1, (a, b) =>  a * b);
            long sum = 0;
            for (int i = 0; i < num.Count; i++)
            {
                long p = prod / num[i];
                sum += rem[i] * Utils.ModInverse(p, num[i]) * p;
            }
            return sum % prod;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            List<int> ids = new List<int>();
            List<int> offs = new List<int>();
            int i = 0;
            foreach (string s in input[1].Split(','))
            {
                if (s != "x")
                {
                    int id = int.Parse(s);
                    ids.Add(id);
                    offs.Add(id - i);
                }
                i++;
            }
            long ans = ChineseRemainder(ids, offs);
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
            int a = 153;
            long b = 471793476184394;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
