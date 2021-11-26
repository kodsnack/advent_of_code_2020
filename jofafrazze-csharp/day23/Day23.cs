using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day23
{
    public class Day23
    {
        readonly static string nsname = typeof(Day23).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 23: Crab Cups - Small game with numbers moving in a circle

        static void StepCrabCups(LinkedList<int> cups, List<LinkedListNode<int>> valueToCup, int iter)
        {
            int maxValue = cups.Count;
            LinkedListNode<int> curCup = cups.First;
            for (int i = 0; i < iter; i++)
            {
                List<int> takeValues = new List<int>();
                var nextCup = curCup.NextOrFirst();
                for (int n = 0; n < 3; n++)
                {
                    var cup = nextCup;
                    nextCup = nextCup.NextOrFirst();
                    takeValues.Add(cup.Value);
                }
                int a = curCup.Value - 1;
                while (a < 1 || takeValues.Contains(a))
                    a = a < 1 ? maxValue : a - 1;
                var destCup = valueToCup[a];
                for (int n = 2; n >= 0; n--)
                {
                    var tCup = valueToCup[takeValues[n]];
                    cups.Remove(tCup);
                    cups.AddAfter(destCup, tCup);
                }
                curCup = curCup.NextOrFirst();
            }
        }

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            LinkedList<int> circularBuffer = new LinkedList<int>(input[0].Select(x => (int)x - '0'));
            int n = circularBuffer.Count;
            var valueToCup = new List<LinkedListNode<int>>(n + 1);
            for (int i = 0; i <= n; i++)
                valueToCup.Add(null);
            var pos = circularBuffer.First;
            for (int i = 1; i <= n; i++)
            {
                valueToCup[pos.Value] = pos;
                pos = pos.Next;
            }
            StepCrabCups(circularBuffer, valueToCup, 100);
            pos = circularBuffer.Find(1);
            var pp = pos.NextOrFirst();
            string ans = "";
            while (pp != pos)
            {
                ans += pp.Value.ToString();
                pp = pp.NextOrFirst();
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            LinkedList<int> circularBuffer = new LinkedList<int>(input[0].Select(x => (int)x - '0'));
            int n = 1_000_000;
            for (int i = 10; i <= n; i++)
                circularBuffer.AddLast(i);
            var valueToCup = new List<LinkedListNode<int>>(n + 1);
            for (int i = 0; i <= n; i++)
                valueToCup.Add(null);
            var pos = circularBuffer.First;
            for (int i = 1; i <= n; i++)
            {
                valueToCup[pos.Value] = pos;
                pos = pos.Next;
            }
            StepCrabCups(circularBuffer, valueToCup, 10_000_000);
            pos = circularBuffer.Find(1);
            var p = pos.NextOrFirst();
            var pp = p.NextOrFirst();
            long ans = p.Value * (long)pp.Value;
            Console.WriteLine("Part B: Result is {0}", ans);
            return ans;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2020 - " + nsname + ":");
            var w = System.Diagnostics.Stopwatch.StartNew();
            PartA();
            PartB();
            w.Stop();
            Console.WriteLine("[Execution took {0} ms]", w.ElapsedMilliseconds);
        }

        public static bool MainTest()
        {
            string a = "47382659";
            long b = 42271866720;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
