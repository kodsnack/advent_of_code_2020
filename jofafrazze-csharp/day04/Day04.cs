using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace day04
{
    public class Day04
    {
        readonly static string nsname = typeof(Day04).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static List<Dictionary<string, string>> ReadInput(string path)
        {
            StreamReader reader = File.OpenText(path);
            List<Dictionary<string, string>> list = new List<Dictionary<string, string>>();
            Dictionary<string, string> dict = new Dictionary<string, string>();
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                if (line == "")
                {
                    list.Add(dict);
                    dict = new Dictionary<string, string>();
                }
                else
                {
                    string[] s = line.Split(' ').ToArray();
                    foreach (string a in s)
                    {
                        string[] b = a.Split(':').ToArray();
                        dict[b[0]] = b[1];
                    }
                }
            }
            if (dict.Count() > 0)
                list.Add(dict);
            return list;
        }
        static Object PartA()
        {
            List<Dictionary<string, string>> input = ReadInput(inputPath);
            int ans = 0;
            foreach(var d in input)
            {
                int n = 0;
                n += d.ContainsKey("byr") ? 1 : 0;
                n += d.ContainsKey("iyr") ? 1 : 0;
                n += d.ContainsKey("eyr") ? 1 : 0;
                n += d.ContainsKey("hgt") ? 1 : 0;
                n += d.ContainsKey("hcl") ? 1 : 0;
                n += d.ContainsKey("ecl") ? 1 : 0;
                n += d.ContainsKey("pid") ? 1 : 0;
                if (n >= 7)
                    ans++;
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            List<Dictionary<string, string>> input = ReadInput(inputPath);
            int ans = 0;
            foreach (var d in input)
            {
                int n = 0;
                if (d.ContainsKey("byr"))
                {
                    int a = int.Parse(d["byr"]);
                    if (a >= 1920 && a <= 2002)
                        n++;
                }
                if (d.ContainsKey("iyr"))
                {
                    int a = int.Parse(d["iyr"]);
                    if (a >= 2010 && a <= 2020)
                        n++;
                }
                if (d.ContainsKey("eyr"))
                {
                    int a = int.Parse(d["eyr"]);
                    if (a >= 2020 && a <= 2030)
                        n++;
                }
                if (d.ContainsKey("hgt"))
                {
                    string s = d["hgt"];
                    int a = 0;
                    if (s.Length > 2)
                        a = int.Parse(s.Remove(s.Length - 2));
                    if (s.EndsWith("cm") && a >= 150 && a <= 193)
                        n++;
                    else if (s.EndsWith("in") && a >= 59 && a <= 76)
                        n++;
                }
                if (d.ContainsKey("hcl"))
                {
                    string s = d["hcl"];
                    if (s.Length == 7 && s[0] == '#')
                    {
                        string hex = new string(s.Where(Uri.IsHexDigit).ToArray());
                        if (hex.Length == 6)
                            n++;
                    }
                }
                if (d.ContainsKey("ecl"))
                {
                    string s = d["ecl"];
                    if (s == "amb" || s == "blu" || s == "brn" || s == "gry" || s == "grn" || s == "hzl" || s == "oth")
                        n++;
                }
                if (d.ContainsKey("pid"))
                {
                    string s = d["pid"];
                    string dec = new string(s.Where(char.IsDigit).ToArray());
                    if (dec.Length == 9)
                        n++;
                }
                if (n >= 7)
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
            int a = 228;
            int b = 175;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
