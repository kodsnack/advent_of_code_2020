using System;
using System.IO;
using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day12
{
    public class Day12
    {
        readonly static string nsname = typeof(Day12).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 12: Rain Risk - Move absolute/relatively in 2D, rotate to another quadrant

        static string cardinals = "NESW";

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            Position pos = new Position();
            int dirIdx = 1;
            foreach (var s in input)
            {
                char c = s[0];
                int n = int.Parse(s.Substring(1));
                if (c == 'F')
                    pos += CoordsXY.directions4[Utils.Modulo(dirIdx, 4)] * n;
                else if (c == 'L')
                    dirIdx -= n / 90;
                else if (c == 'R')
                    dirIdx += n / 90;
                else 
                    pos += CoordsXY.directions4[cardinals.IndexOf(c)] * n;
            }
            int ans = pos.ManhattanDistance();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            Position wpos = new Position(10, 1);
            Position spos = new Position();
            foreach (var s in input)
            {
                char c = s[0];
                int n = int.Parse(s.Substring(1));
                if (c == 'F')
                    spos += wpos * n;
                else if (c == 'L')
                    wpos = Position.Rotate4Steps(wpos, -n / 90);
                else if (c == 'R')
                    wpos = Position.Rotate4Steps(wpos, n / 90);
                else
                    wpos += CoordsXY.directions4[cardinals.IndexOf(c)] * n;
            }
            int ans = spos.ManhattanDistance();
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
            int a = 2280;
            int b = 38693;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
