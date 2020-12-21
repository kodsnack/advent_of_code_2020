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

        static Dictionary<int, (List<int> r1, List<int> r2)> rules = new Dictionary<int, (List<int>, List<int>)>();

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
                    var m = v[1].Split("|".ToCharArray());
                    var m1 = m[0].Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    var m2 = m.Count() > 1 ? m[1].Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries) : new string[] { };
                    rules[int.Parse(v[0])] = (m1[0][0] == '"') 
                        ? (new List<int> { -m1[0][1] }, new List<int>())
                        : (m1.Select(int.Parse).ToList(), m2.Select(int.Parse).ToList());
                }
                else if (phase == 1)
                    list.Add(line);
            }
            return list;
        }

        struct Pos { public bool ok; public int n; public Pos(bool b, int i) { ok = b; n = i; } }

        static Pos Comb(List<Pos> m)
        {
            return (m.Aggregate((a, b) => new Pos(a.ok && b.ok, b.n)));
        }
        static Pos Select(Pos a, Pos b)
        {
            return a.ok ? a : b;
        }
        static Pos Match(string s, int r, int n)
        {
            List<Pos> DoMatch(List<int> rs)
            {
                Pos p = new Pos(false, n);
                return rs.Select(a => p = Match(s, a, p.n)).ToList();
            }
            if (r < 0 || n >= s.Length)
                return new Pos(false, n);
            var (r1, r2) = rules[r];
            var m1 = DoMatch(r1);
            var m2 = DoMatch(r2);
            bool ok = s[n] == -r1[0];
            if (r2.Count == 0)
                return (r1[0] < 0) ? new Pos(ok, ok ? n + 1 : n) : Comb(m1);
            else
                return Select(Comb(m1), Comb(m2));
        }

        static Object PartA()
        {
            var input = ReadInput(inputPath);
            int ans = input.Select(s => (s, m: Match(s, 0, 0)))
                .Where(z => z.m.ok && z.m.n == z.s.Length).Count();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static List<int> CountMatches(string s, int rule, int n)
        {
            var pos = new List<int>();
            Pos p = new Pos(true, n);
            while (p.ok && p.n < s.Length)
            {
                p = Match(s, rule, p.n);
                if (p.ok)
                    pos.Add(p.n);
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
