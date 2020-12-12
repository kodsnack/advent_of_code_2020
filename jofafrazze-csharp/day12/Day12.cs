using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day12
{
    public class Day12
    {
        readonly static string nsname = typeof(Day12).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

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
                else if (c == 'N')
                    pos += CoordsXY.goUp * n;
                else if (c == 'E')
                    pos += CoordsXY.goRight * n;
                else if (c == 'S')
                    pos += CoordsXY.goDown * n;
                else if (c == 'W')
                    pos += CoordsXY.goLeft * n;
            }
            int ans = pos.ManhattanDistance();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Position Turn(Position p, int n)
        {
            n = Utils.Modulo(n, 4);
            Position r = new Position();
            if (n == 0)
            {
                r = p;
            }
            else if (n == 1)
            {
                r.x = p.y;
                r.y = -p.x;
            }
            else if (n == 2)
            {
                r.x = -p.x;
                r.y = -p.y;
            }
            else if (n == 3)
            {
                r.x = -p.y;
                r.y = p.x;
            }
            return r;
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
                    wpos = Turn(wpos, -n / 90);
                else if (c == 'R')
                    wpos = Turn(wpos, n / 90);
                else if (c == 'N')
                    wpos += CoordsXY.goUp * n;
                else if (c == 'E')
                    wpos += CoordsXY.goRight * n;
                else if (c == 'S')
                    wpos += CoordsXY.goDown * n;
                else if (c == 'W')
                    wpos += CoordsXY.goLeft * n;
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
