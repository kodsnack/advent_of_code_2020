using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace day19
{
    public class Day19
    {
        readonly static string nsname = typeof(Day19).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static Dictionary<int, ((int a, int b) r1, (int a, int b, int c) r2)> rules = new Dictionary<int, ((int, int), (int, int, int))>();

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
                    {
                        int a = -m1[0][1];
                        rules[int.Parse(v[0])] = ((a, 255), (255, 255, 255));
                    }
                    else
                    {
                        bool second = m.Count() > 1;
                        var m2 = second ? m[1].Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries) : null;
                        int n2 = second ? m2.Count() : 0;
                        int b1 = (int)int.Parse(m1[0]);
                        int b2 = (int)(n1 > 1 ? int.Parse(m1[1]) : 255);
                        int b3 = (int)(n2 > 0 ? int.Parse(m2[0]) : 255);
                        int b4 = (int)(n2 > 1 ? int.Parse(m2[1]) : 255);
                        rules[int.Parse(v[0])] = ((b1, b2), (b3, b4, 255));
                    }
                }
                else if (phase == 1)
                    list.Add(line);
            }
            return list;
        }

        static (bool ok, int x) Match(string s, int rule1, int x)
        {
            int a1 = rules[rule1].r1.a;
            int a2 = rules[rule1].r1.b;
            int b1 = rules[rule1].r2.a;
            int b2 = rules[rule1].r2.b;
            int b3 = rules[rule1].r2.c;
            if (x >= s.Length)
                return (false, x);
            else if (b1 == 255)
            {
                if (a1 < 0 && a2 == 255)
                    return (s[x] == -a1, s[x] == -a1 ? x + 1 : x);
                else if (a2 == 255)
                    return Match(s, a1, x);
                else
                {
                    var m1 = Match(s, a1, x);
                    var m2 = Match(s, a2, m1.x);
                    return (m1.ok && m2.ok, m2.x);
                }
            }
            else
            {
                if (a2 == 255 && b2 == 255)
                {
                    var m1 = Match(s, a1, x);
                    var n2 = Match(s, b1, x);
                    return m1.ok ? m1 : n2;
                }
                else if (b2 == 255)
                {
                    var m1 = Match(s, a1, x);
                    var m2 = Match(s, a2, m1.x);
                    bool d = m1.ok && m2.ok;
                    return d ? (d, m2.x) : Match(s, b1, x);
                }
                else if (a2 == 255)
                {
                    var m1 = Match(s, a1, x);
                    if (!m1.ok)
                    {
                        var n1 = Match(s, b1, x);
                        var n2 = Match(s, b2, n1.x);
                        return (n1.ok && n2.ok, n2.x);
                    }
                    else
                        return m1;
                }
                else if (b3 == 255)
                {
                    var m1 = Match(s, a1, x);
                    var m2 = Match(s, a2, m1.x);
                    if (m1.ok && m2.ok)
                        return (m1.ok && m2.ok, m2.x);
                    else
                    {
                        var n1 = Match(s, b1, x);
                        var n2 = Match(s, b2, n1.x);
                        return (n1.ok && n2.ok, n2.x);
                    }
                }
                else
                {
                    var m1 = Match(s, a1, x);
                    var m2 = Match(s, a2, m1.x);
                    if (m1.ok && m2.ok)
                        return (m1.ok && m2.ok, m2.x);
                    else
                    {
                        var n1 = Match(s, b1, x);
                        var n2 = Match(s, b2, n1.x);
                        var n3 = Match(s, b3, n2.x);
                        return (n1.ok && n2.ok && n3.ok, n3.x);
                    }
                }
            }
        }

        static Object PartA()
        {
            var input = ReadInput(inputPath);
            int ans = 0;
            foreach (var s in input)
            {
                var m = Match(s, 0, 0);
                if (m.ok && m.x == s.Length)
                    ans++;
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadInput(inputPath);
            rules[8] = ((42, 255), (42, 8, 255));
            rules[11] = ((42, 31), (42, 11, 31));
            int ans = 0;
            foreach (var s in input)
            {
                var m = Match(s, 0, 0);
                if (m.ok && m.x == s.Length)
                    ans++;
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
            int a = 42;
            int b = 4711;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
