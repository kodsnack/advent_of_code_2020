using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

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

        public static bool CanContain(Bag bag, Bag inside)
        {
            return bag.children.Where(x => x.Key == inside || CanContain(x.Key, inside)).Count() > 0;
        }

        static Object PartA()
        {
            var bags = ReadInput(inputPath);
            Bag ourBag = bags["shiny gold"];
            int ans = bags.Where(x => CanContain(x.Value, ourBag)).Count();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        public static long BagsContained(Bag bag)
        {
            return bag.children.Select(x => x.Value * (BagsContained(x.Key) + 1)).Sum();
        }

        static Object PartB()
        {
            var bags = ReadInput(inputPath);
            Bag ourBag = bags["shiny gold"];
            long ans = BagsContained(ourBag);
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
            int a = 224;
            long b = 1488;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
