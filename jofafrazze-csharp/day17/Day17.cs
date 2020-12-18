using System;
using System.Collections.Generic;
using System.IO;
using AdventOfCode;

namespace day17
{
    public class Day17
    {
        readonly static string nsname = typeof(Day17).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static HashSet<(int, int, int)> GetNeighbours((int x, int y, int z) p)
        {
            var m = new HashSet<(int, int, int)>();
            int[] offs = new int[] { -1, 0, 1 };
            foreach (int x in offs)
                foreach (int y in offs)
                    foreach (int z in offs)
                        if (x != 0 || y != 0 || z != 0)
                            m.Add((p.x + x, p.y + y, p.z + z));
            return m;
        }

        static HashSet<(int, int, int)> StepMap(HashSet<(int x, int y, int z)> m)
        {
            var mNext = new HashSet<(int, int, int)>(m);
            var mNeigh = new HashSet<(int, int, int)>(m);
            foreach (var p in m)
                mNeigh.UnionWith(GetNeighbours(p));
            foreach (var p in mNeigh)
            {
                int n = 0;
                foreach (var q in GetNeighbours(p))
                    if (m.Contains(q))
                        n++;
                if (m.Contains(p) && (n < 2 || n > 3))
                    mNext.Remove(p);
                if (!m.Contains(p) && n == 3)
                    mNext.Add(p);
            }
            return mNext;
        }

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            var m = new HashSet<(int x, int y, int z)>();
            int k = input.Count;
            for (int y = 0; y < k; y++)
                for (int x = 0; x < k; x++)
                    if (input[y][x] == '#')
                        m.Add((x, y, 0));
            for (int i = 0; i < 6; i++)
                m = StepMap(m);
            int ans = m.Count;
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static HashSet<(int, int, int, int)> GetNeighbours((int x, int y, int z, int w) p)
        {
            var m = new HashSet<(int, int, int, int)>();
            int[] offs = new int[] { -1, 0, 1 };
            foreach (int x in offs)
                foreach (int y in offs)
                    foreach (int z in offs)
                        foreach (int w in offs)
                            if (x != 0 || y != 0 || z != 0 || w != 0)
                                m.Add((p.x + x, p.y + y, p.z + z, p.w + w));
            return m;
        }

        static HashSet<(int, int, int, int)> StepMap(HashSet<(int x, int y, int z, int w)> m)
        {
            var mNext = new HashSet<(int, int, int, int)>(m);
            var mNeigh = new HashSet<(int, int, int, int)>(m);
            foreach (var p in m)
                mNeigh.UnionWith(GetNeighbours(p));
            foreach (var p in mNeigh)
            {
                int n = 0;
                foreach (var q in GetNeighbours(p))
                    if (m.Contains(q))
                        n++;
                if (m.Contains(p) && (n < 2 || n > 3))
                    mNext.Remove(p);
                if (!m.Contains(p) && n == 3)
                    mNext.Add(p);
            }
            return mNext;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            var m = new HashSet<(int x, int y, int z, int w)>();
            int k = input.Count;
            for (int y = 0; y < k; y++)
                for (int x = 0; x < k; x++)
                    if (input[y][x] == '#')
                        m.Add((x, y, 0, 0));
            for (int i = 0; i < 6; i++)
                m = StepMap(m);
            int ans = m.Count;
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
            int a = 280;
            int b = 1696;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
