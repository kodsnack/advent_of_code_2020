using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day16
{
    public class Day16
    {
        readonly static string nsname = typeof(Day16).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static List<((int, int), (int, int))> rules = new List<((int, int), (int, int))>();
        static List<int> myTicket = new List<int>();
        static List<List<int>> validTickets = new List<List<int>>();

        static List<List<int>> ReadInput(string path)
        {
            var strs = ReadIndata.Strings(path);
            var list = new List<List<int>>();
            int phase = 0;
            foreach (var line in strs)
            {
                if (line == "")
                    phase++;
                else if (phase == 0)
                {
                    var v = line.Split(':')[1].Split(" -or".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    rules.Add((((int.Parse(v[0]), int.Parse(v[1]))), ((int.Parse(v[2]), int.Parse(v[3])))));
                }
                else if (phase == 1 && !line.Contains(':'))
                    myTicket = line.Split(',').Select(int.Parse).ToList();
                else if (phase == 2 && !line.Contains(':'))
                    list.Add(line.Split(',').Select(int.Parse).ToList());
            }
            return list;
        }

        static Object PartA()
        {
            var input = ReadInput(inputPath);
            int ans = 0;
            foreach (var t in input)
            {
                bool tOk = true;
                foreach (var f in t)
                {
                    bool ok = false;
                    foreach (var r in rules)
                        if ((f >= r.Item1.Item1 && f <= r.Item1.Item2) || (f >= r.Item2.Item1 && f <= r.Item2.Item2))
                            ok = true;
                    if (!ok)
                        ans += f;
                    tOk &= ok;
                }
                if (tOk)
                    validTickets.Add(t);
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            int n = rules.Count;
            var rulesForField = new Dictionary<int, HashSet<int>>();
            for (int fi = 0; fi < n; fi++)
            {
                var rulesOk = Enumerable.Range(0, n).ToHashSet();
                foreach (var ticket in validTickets)
                {
                    var f = ticket[fi];
                    int ri = 0;
                    foreach (var r in rules)
                    {
                        if (!((f >= r.Item1.Item1 && f <= r.Item1.Item2) || (f >= r.Item2.Item1 && f <= r.Item2.Item2)))
                            rulesOk.Remove(ri);
                        ri++;
                    }
                }
                rulesForField[fi] = rulesOk;
            }
            var ruleToField = new List<int>(Enumerable.Repeat(-1, n));
            for (int i = 0; i < n; i++)
            {
                (int field, var rules) = rulesForField.Where(x => x.Value.Count == 1).FirstOrDefault();
                int rule = rules.FirstOrDefault();
                ruleToField[rule] = field;
                foreach ((var _, var r) in rulesForField)
                    r.Remove(rule);
            }
            long ans = 1;
            for (int i = 0; i < 6; i++)
                ans *= myTicket[ruleToField[i]];
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
            int a = 29878;
            long b = 855438643439;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
