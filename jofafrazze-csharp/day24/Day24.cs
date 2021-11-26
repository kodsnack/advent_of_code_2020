using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day24
{
    public class Day24
    {
        readonly static string nsname = typeof(Day24).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 24: Lobby Layout - Parse without delims, handle hex pattern

        static HashSet<Position> black = new HashSet<Position>();

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            Regex re = new Regex(@"(e|se|sw|w|nw|ne)");
            foreach (var s in input)
            {
                var pos = new Position();
                foreach (Match match in re.Matches(s))
                    pos += CoordsHex.directionsWide[match.Value];
                if (black.Contains(pos))
                    black.Remove(pos);
                else
                    black.Add(pos);
            }
            int ans = black.Count();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static HashSet<Position> GetNeighbours(Position p)
        {
            var m = new HashSet<Position>();
            foreach (var d in CoordsHex.directionsWide.Values)
                m.Add(p + d);
            return m;
        }

        static Object PartB()
        {
            for (int i = 0; i < 100; i++)
            {
                var nextBlack = new HashSet<Position>(black);
                var mNeigh = new HashSet<Position>(black);
                foreach (var p in black)
                    mNeigh.UnionWith(GetNeighbours(p));
                foreach (var p in mNeigh)
                {
                    int n = 0;
                    foreach (var d in CoordsHex.directionsWide.Values)
                        if (black.Contains(p + d))
                            n++;
                    if (black.Contains(p) && (n == 0 || n > 2))
                        nextBlack.Remove(p);
                    else if (!black.Contains(p) && n == 2)
                        nextBlack.Add(p);
                }
                black = nextBlack;
            }
            int ans = black.Count();
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
            int a = 523;
            int b = 4225;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
