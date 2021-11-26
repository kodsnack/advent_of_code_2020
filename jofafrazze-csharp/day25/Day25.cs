using System;
using System.IO;
using AdventOfCode;

namespace day25
{
    public class Day25
    {
        readonly static string nsname = typeof(Day25).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 25: Combo Breaker - Small calculations

        static int CalcLoopSize(int subjectNumber, long target)
        {
            long value = 1;
            int i = 0;
            for (i = 0; value != target; i++)
            {
                value *= subjectNumber;
                value %= 20201227;
            }
            return i;
        }

        static long Calc(int subjectNumber, long loopSize)
        {
            long value = 1;
            for (int i = 0; i < loopSize; i++)
            {
                value *= subjectNumber;
                value %= 20201227;
            }
            return value;
        }

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            int cardPk = int.Parse(input[0]);
            int doorPk = int.Parse(input[1]);
            int cardLoopSize = CalcLoopSize(7, cardPk);
            //int doorLoopSize = CalcLoopSize(7, doorPk);
            long ans = Calc(doorPk, cardLoopSize);
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2020 - " + nsname + ":");
            var w = System.Diagnostics.Stopwatch.StartNew();
            PartA();
            w.Stop();
            Console.WriteLine("[Execution took {0} ms]", w.ElapsedMilliseconds);
        }

        public static bool MainTest()
        {
            long a = 3803729;
            return PartA().Equals(a);
        }
    }
}
