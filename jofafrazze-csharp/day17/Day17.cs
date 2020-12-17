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

        static HashSet<(int, int, int)> GetNeighbours3D((int x, int y, int z) p)
        {
            var m = new HashSet<(int, int, int)>();
            int[] offs = new int[] { -1, 0, 1 };
            foreach (int i in offs)
                foreach (int j in offs)
                    foreach (int k in offs)
                        if (i != 0 || j != 0 || k != 0)
                            m.Add((p.x + i, p.y + j, p.z + k));
            return m;
        }

        static HashSet<(int, int, int)> StepMap3D(HashSet<(int x, int y, int z)> m)
        {
            var mNext = new HashSet<(int, int, int)>(m);
            HashSet<(int x, int y, int z)> mNeigh = new HashSet<(int x, int y, int z)>(m);
            foreach (var p in m)
                mNeigh.UnionWith(GetNeighbours3D(p));
            foreach (var p in mNeigh)
            {
                int n = 0;
                foreach (var q in GetNeighbours3D(p))
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
                m = StepMap3D(m);
            int ans = m.Count;
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static HashSet<(int, int, int, int)> GetNeighbours4D((int x, int y, int z, int w) p)
        {
            var m = new HashSet<(int, int, int, int)>();
            int[] offs = new int[] { -1, 0, 1 };
            foreach (int i in offs)
                foreach (int j in offs)
                    foreach (int k in offs)
                        foreach (int l in offs)
                            if (i != 0 || j != 0 || k != 0 || l != 0)
                                m.Add((p.x + i, p.y + j, p.z + k, p.w + l));
            return m;
        }

        static HashSet<(int, int, int, int)> StepMap4D(HashSet<(int x, int y, int z, int w)> m)
        {
            var mNext = new HashSet<(int, int, int, int)>(m);
            HashSet<(int x, int y, int z, int w)> mNeigh = new HashSet<(int x, int y, int z, int w)>(m);
            foreach (var p in m)
                mNeigh.UnionWith(GetNeighbours4D(p));
            foreach (var p in mNeigh)
            {
                int n = 0;
                foreach (var q in GetNeighbours4D(p))
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
                m = StepMap4D(m);
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
