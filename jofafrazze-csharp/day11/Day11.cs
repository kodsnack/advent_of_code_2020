using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;
using Position = AdventOfCode.GenericPosition2D<int>;

namespace day11
{
    public class Day11
    {
        readonly static string nsname = typeof(Day11).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 11: Seating System - Game of life with two different iteration rules 

        static Map StepMap(Map m)
        {
            int w = m.width;
            int h = m.height;
            Map mNext = new Map(m);
            for (int y = 0; y < h; y++)
            {
                for (int x = 0; x < w; x++)
                {
                    var p = new Position(x, y);
                    var c = m[p];
                    if (c != '.')
                    {
                        int n = CoordsRC.directions8.Where(d => m.HasPosition(p + d) && m[p + d] == '#').Count();
                        if (n == 0 && c == 'L')
                            mNext[p] = '#';
                        if (n >= 4 && c == '#')
                            mNext[p] = 'L';
                    }
                }
            }
            return mNext;
        }

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            Map m = Map.Build(input);
            Map m2 = new Map(m);
            //m.Print();
            int n = 0;
            do
            {
                m = m2;
                m2 = StepMap(m);
                //m2.Print();
                n++;
            }
            while (m != m2);
            int ans = m.data.Cast<char>().Where(x => x == '#').Count();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Map StepMapB(Map m)
        {
            int w = m.width;
            int h = m.height;
            Map mNext = new Map(m);
            for (int y = 0; y < h; y++)
            {
                for (int x = 0; x < w; x++)
                {
                    var p = new Position(x, y);
                    var c = m[p];
                    if (c != '.')
                    {
                        int n = 0;
                        foreach (var d in CoordsRC.directions8)
                        {
                            bool done = false;
                            int k = 1;
                            while (!done)
                            {
                                var pd = p + d * k;
                                done = !m.HasPosition(pd) || (m[pd] == '#' || m[pd] == 'L');
                                if (m.HasPosition(pd) && (m[pd] == '#'))
                                    n++;
                                k++;
                            }
                        }
                        if (n == 0 && c == 'L')
                            mNext[p] = '#';
                        if (n >= 5 && c == '#')
                            mNext[p] = 'L';
                    }
                }
            }
            return mNext;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            Map m = Map.Build(input);
            Map m2 = new Map(m);
            int n = 0;
            do
            {
                m = m2;
                m2 = StepMapB(m);
                n++;
            }
            while (m != m2);
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
