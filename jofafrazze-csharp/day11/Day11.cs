using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day11
{
    public class Day11
    {
        readonly static string nsname = typeof(Day11).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static readonly Position goUpL = new Position(-1, -1);
        static readonly Position goUp = new Position(0, -1);
        static readonly Position goUpR = new Position(1, -1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly Position goRight = new Position(1, 0);
        static readonly Position goDownL = new Position(-1, 1);
        static readonly Position goDown = new Position(0, 1);
        static readonly Position goDownR = new Position(1, 1);
        static readonly List<Position> directions = new List<Position>()
        {
            goUp, goRight, goDown, goLeft, goUpL, goUpR, goDownL, goDownR
        };

        static Map BuildMap(List<string> list)
        {
            int w = list[0].Length;
            int h = list.Count;
            Map m = new Map(w, h, new Position(0, 0));
            for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                    m.data[x, y] = list[y][x];
            return m;
        }

        static Map StepMap(Map m)
        {
            int w = m.width;
            int h = m.height;
            Map a = new Map(m);
            for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                {
                    if (m.data[x, y] != '.')
                    {
                        int n = 0;
                        foreach (var d in directions)
                        {
                            var p = new Position(x, y) + d;
                            if (m.HasPosition(p) && m.data[p.x, p.y] == '#')
                                n++;
                        }
                        if (n == 0 && m.data[x, y] == 'L')
                            a.data[x, y] = '#';
                        if (n >= 4 && m.data[x, y] == '#')
                            a.data[x, y] = 'L';
                    }
                }
            return a;
        }

        static Object PartA()
        {
            var input = ReadInputs.ReadStrings(inputPath);
            Map m = BuildMap(input);
            //m.Print();
            int n = 0;
            while (true)
            {
                Map m2 = StepMap(m);
                n++;
                //m2.Print();
                if (m.data.Cast<char>().SequenceEqual(m2.data.Cast<char>()))
                    break;
                m = m2;
            }
            int ans = m.data.Cast<char>().Where(x => x == '#').Count();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Map StepMapB(Map m)
        {
            int w = m.width;
            int h = m.height;
            Map a = new Map(m);
            for (int y = 0; y < h; y++)
                for (int x = 0; x < w; x++)
                {
                    if (m.data[x, y] != '.')
                    {
                        int n = 0;
                        foreach (var d in directions)
                        {
                            bool done = false;
                            int k = 1;
                            while (!done)
                            {
                                var p = new Position(x, y) + d * k;
                                if (m.HasPosition(p))
                                {
                                    var ch = m.data[p.x, p.y];
                                    if (ch == 'L' || ch == '#')
                                        done = true;
                                    if (ch == '#')
                                        n++;
                                }
                                else
                                    done = true;
                                k++;
                            }
                        }
                        if (n == 0 && m.data[x, y] == 'L')
                            a.data[x, y] = '#';
                        if (n >= 5 && m.data[x, y] == '#')
                            a.data[x, y] = 'L';
                    }
                }
            return a;
        }

        static Object PartB()
        {
            var input = ReadInputs.ReadStrings(inputPath);
            Map m = BuildMap(input);
            //m.Print();
            int n = 0;
            while (true)
            {
                Map m2 = StepMapB(m);
                n++;
                //m2.Print();
                if (m.data.Cast<char>().SequenceEqual(m2.data.Cast<char>()))
                    break;
                m = m2;
            }
            int ans = m.data.Cast<char>().Where(x => x == '#').Count();
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
            int a = 2406;
            int b = 2149;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
