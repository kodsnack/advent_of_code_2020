using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
using Position = AdventOfCode.GenericPosition3D<int>;

namespace day17
{
    public class Day17
    {
        readonly static string nsname = typeof(Day17).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static Map3D StepMap(Map3D m)
        {
            int w = m.width;
            int h = m.height;
            int d = m.depth;
            Map3D mNext = new Map3D(m);
            for (int z = 0; z < d; z++)
            {
                for (int y = 0; y < h; y++)
                {
                    for (int x = 0; x < w; x++)
                    {
                        var p = new Position(x, y, z);
                        var c = m[p];
                        int n = CoordsRCD.directions26.Where(q => m.HasPosition(p + q) && m[p + q] == '#').Count();
                        if (c == '#' && (n < 2 || n > 3))
                            mNext[p] = '.';
                        if (c != '#' && n == 3)
                            mNext[p] = '#';
                    }
                }
            }
            return mNext;
        }

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            int k = input.Count + 2 * 6;
            Map3D m = Map3D.BuildZ0(k, input);
            for (int i = 0; i < 6; i++)
            {
                //Console.WriteLine("Iteration {0}:", i);
                //m.Print();
                m = StepMap(m);
            }
            int ans = m.data.Cast<char>().Where(x => x == '#').Count();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static HashSet<(int, int, int, int)> Get80Neighbours((int x, int y, int z, int w) p)
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
        static HashSet<(int, int, int, int)> StepHashMap(HashSet<(int x, int y, int z, int w)> m)
        {
            int[] offs = new int[] { -1, 0, 1 };
            var mNext = new HashSet<(int, int, int, int)>(m);
            HashSet<(int x, int y, int z, int w)> mNeigh = new HashSet<(int x, int y, int z, int w)>(m);
            foreach (var p in m)
                mNeigh.UnionWith(Get80Neighbours(p));
            foreach (var p in mNeigh)
            {
                int n = 0;
                foreach (int i in offs)
                    foreach (int j in offs)
                        foreach (int k in offs)
                            foreach (int l in offs)
                                if (i != 0 || j != 0 || k != 0 || l != 0)
                                    if (m.Contains((p.x + i, p.y + j, p.z + k, p.w + l)))
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
            Console.WriteLine("Hypercubes: {0}", m.Count);
            for (int i = 0; i < 6; i++)
            {
                m = StepHashMap(m);
                Console.WriteLine("Hypercubes {0}: {1}", i, m.Count);
            }
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
