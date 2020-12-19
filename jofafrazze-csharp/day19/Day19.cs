using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day19
{
    public class Day19
    {
        readonly static string nsname = typeof(Day19).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static Dictionary<int, ((int a, int b) r1, (int a, int b) r2)> rules = new Dictionary<int, ((int, int), (int, int))>();

        static List<string> ReadInput(string path)
        {
            var strs = ReadIndata.Strings(path);
            var list = new List<string>();
            int phase = 0;
            foreach (var line in strs)
            {
                if (line == "")
                    phase++;
                else if (phase == 0)
                {
                    var v = line.Split(':');
                    var m = v[1].Split("|".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    var m1 = m[0].Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    int n1 = m1.Count();
                    if (m1[0][0] == '"')
                        rules[int.Parse(v[0])] = ((-m1[0][1], -1), (-1, -1));
                    else
                    {
                        bool second = m.Count() > 1;
                        var m2 = second ? m[1].Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries) : null;
                        int n2 = second ? m2.Count() : 0;
                        int a = int.Parse(m1[0]);
                        int b = n1 > 1 ? int.Parse(m1[1]) : -1;
                        int c = n2 > 0 ? int.Parse(m2[0]) : -1;
                        int d = n2 > 1 ? int.Parse(m2[1]) : -1;
                        rules[int.Parse(v[0])] = ((a, b), (c, d));
                    }
                }
                else if (phase == 1)
                    list.Add(line);
            }
            return list;
        }

        static (bool ok, int x) Comb((bool ok, int x) a, (bool ok, int x) b)
        {
            return (a.ok && b.ok, b.x);
        }
        static (bool ok, int x) Select((bool ok, int x) a, (bool ok, int x) b)
        {
            return a.ok ? a : b;
        }
        static (bool ok, int x) Match(string s, int r, int x)
        {
            if (r < 0 || x >= s.Length)
                return (false, x);
            int a1 = rules[r].r1.a;
            int a2 = rules[r].r1.b;
            int b1 = rules[r].r2.a;
            int b2 = rules[r].r2.b;
            var m1 = Match(s, a1, x);
            var m2 = Match(s, a2, m1.x);
            var n1 = Match(s, b1, x);
            var n2 = Match(s, b2, n1.x);
            if (b1 < 0)
            {
                if (a1 < 0 && a2 < 0)
                    return (s[x] == -a1, s[x] == -a1 ? x + 1 : x);
                else if (a2 < 0)
                    return m1;
                else
                    return Comb(m1, m2);
            }
            else
            {
                if (a2 < 0 && b2 < 0)
                    return Select(m1, n1);
                else if (b2 < 0)
                    return Select(n1, Comb(m1, m2));
                else if (a2 < 0)
                    return Select(m1, Comb(n1, n2));
                else
                    return Select(Comb(m1, m2), Comb(n1, n2));
            }
        }

        static Object PartA()
        {
            var input = ReadInput(inputPath);
            int ans = input.Select(s => (s, m: Match(s, 0, 0)))
                .Where(b => b.m.ok && b.m.x == b.s.Length).Count();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static List<int> CountMatches(string s, int rule, int x)
        {
            var pos = new List<int>();
            (bool ok, int x) p = (true, x);
            while (p.ok && p.x < s.Length)
            {
                p = Match(s, rule, p.x);
                if (p.ok)
                    pos.Add(p.x);
            }
            return pos;
        }

        static Object PartB()
        {
            var input = ReadInput(inputPath);
            //rules[8] = ((42, -1), (42, 8));
            //rules[11] = ((42, 31), (42, 11, 31));
            int ans = 0;
            foreach (var s in input)
            {
                bool good = false;
                var pos1 = CountMatches(s, 42, 0);
                for (int i = 1; !good && pos1.Count > 0 && (i <= pos1.Count); i++)
                {
                    var pos2 = CountMatches(s, 31, pos1[i - 1]);
                    good = pos2.LastOrDefault() == s.Length && pos1.Count > pos2.Count;
                }
                ans += good ? 1 : 0;
            }
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
            int a = 226;
            int b = 355;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
