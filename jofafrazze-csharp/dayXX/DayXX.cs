using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace dayXX
{
    public class DayXX
    {
        readonly static string nsname = typeof(DayXX).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day XX: 

        static Object PartA()
        {
            var input = ReadIndata.Ints(inputPath);
            int ans = 0;
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            int ans = 0;
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
            int a = 42;
            int b = 4711;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
