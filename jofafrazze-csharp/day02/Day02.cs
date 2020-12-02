using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace day02
{
    public class Day02
    {
        readonly static string nsname = typeof(Day02).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        struct DbEntry
        {
            public int n1;
            public int n2;
            public char c;
            public string data;
        };

        static List<DbEntry> ReadInput(string path)
        {
            StreamReader reader = File.OpenText(path);
            List<DbEntry> list = new List<DbEntry>();
            string line;
            Regex parts = new Regex(@"^(\d+)-(\d+) ([a-z])\: ([a-z]+)");
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = parts.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    DbEntry d = new DbEntry();
                    d.n1 = int.Parse(groups[1].Value);
                    d.n2 = int.Parse(groups[2].Value);
                    d.c = groups[3].Value[0];
                    d.data = groups[4].Value;
                    list.Add(d);
                }
            }
            return list;
        }

        static List<DbEntry> ReadInput2(string path)
        {
            StreamReader reader = File.OpenText(path);
            List<DbEntry> list = new List<DbEntry>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                DbEntry d = new DbEntry();
                string[] s = line.Split(' ').ToArray();
                string[] t = s[0].Split('-').ToArray();
                d.n1 = int.Parse(t[0]);
                d.n2 = int.Parse(t[1]);
                d.c = s[1][0];
                d.data = s[2];
                list.Add(d);
            }
            return list;
        }

        static Object PartA()
        {
            List<DbEntry> input = ReadInput(inputPath);
            int ans = 0;
            foreach (DbEntry d in input)
            {
                int n = d.data.Count(x => x == d.c);
                if (n >= d.n1 && n <= d.n2)
                    ans++;
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            List<DbEntry> input = ReadInput(inputPath);
            int ans = 0;
            foreach (DbEntry d in input)
            {
                if (d.data[d.n1 - 1] == d.c ^ d.data[d.n2 - 1] == d.c)
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
            int a = 493;
            int b = 593;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
