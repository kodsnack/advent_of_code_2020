using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace day09
{
    public class Day09
    {
        readonly static string nsname = typeof(Day09).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static readonly int z = 25;
        static Object PartA()
        {
            var input = ReadInputs.ReadLongs(inputPath);
            long ans = 0;
            for (int t = z; z < input.Count; t++)
            {
                var pre = input.Skip(t - z).Take(z).ToList();
                long tgt = input[t];
                bool found = false;
                for (int i = 0; i < z; i++)
                {
                    long a = pre[i];
                    for (int j = 0; j < z; j++)
                    {
                        if (i != j)
                        {
                            if (a + pre[j] == tgt)
                                found = true;
                        }

                    }
                }
                if (!found)
                {
                    ans = tgt;
                    break;
                }
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadInputs.ReadLongs(inputPath);
            long ans = 0;
            const long tgt = 167829540;
            for (int t = 0; t < input.Count; t++)
            {
                long sum = 0;
                int n = 0;
                while ((sum < tgt) || (n < 2))
                {
                    sum += input[t + n];
                    n++;
                }
                if (sum == tgt)
                {
                    var r = input.Skip(t).Take(n).ToList();
                    ans = r.Min() + r.Max();
                    break;
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
            long a = 167829540;
            long b = 28045630;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
