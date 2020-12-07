using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;

namespace day07
{
    public class Day07
    {
        readonly static string nsname = typeof(Day07).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        public class Bag
        {
            public string name;
            public Dictionary<Bag, int> children;
            public Bag(string n)
            {
                name = n;
                children = new Dictionary<Bag, int>();
            }
        };

        public static Dictionary<string, Bag> ReadInput(string path)
        {
            StreamReader reader = File.OpenText(path);
            var bags = new Dictionary<string, Bag>();
            Bag GetBag(string s)
            {
                if (bags.ContainsKey(s))
                    return bags[s];
                Bag n = new Bag(s);
                bags[s] = n;
                return n;
            }
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                string[] v = line.Split(" bags contain ").ToArray();
                string[] s = v[1].Split(", ").ToArray();
                Bag b = GetBag(v[0]);
                if (!s[0].Contains("no other"))
                {
                    for (int i = 0; i < s.Length; i++)
                    {
                        Regex parts = new Regex(@"(\d+) (\w+ \w+)");
                        MatchCollection matches = parts.Matches(s[i]);
                        if (matches.Count > 0)
                        {
                            GroupCollection groups = matches[0].Groups;
                            string n = groups[2].Value;
                            Bag c = GetBag(n);
                            b.children[c] = int.Parse(groups[1].Value);
                        }
                    }
                }
            }
            return bags;
        }

        public static Dictionary<string, Bag> bags;

        public static bool CanContain(Bag bag, Bag c)
        {
            if (bag.children.ContainsKey(c))
                return true;
            foreach (var v in bag.children)
                if (CanContain(v.Key, c))
                    return true;
            return false;
        }

        static Object PartA()
        {
            bags = ReadInput(inputPath);
            int ans = 0;
            Bag ourBag = bags["shiny gold"];
            foreach (var bag in bags)
            {
                if (CanContain(bag.Value, ourBag))
                    ans++;
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        public static long ContainsAmount(Bag bag)
        {
            if (bag.children.Count() == 0)
                return 0;
            long n = 0;
            foreach (var v in bag.children)
                if (v.Key.name != bag.name)
                    n += v.Value * (1 + ContainsAmount(v.Key));
            return n;
        }

        static Object PartB()
        {
            bags = ReadInput(inputPath);
            Bag ourBag = bags["shiny gold"];
            long ans = ContainsAmount(ourBag);
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
