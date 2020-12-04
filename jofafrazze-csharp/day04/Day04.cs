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
                foreach ((string k, string v) in d)
                {
                    if (k == "byr")
                    {
                        int a = int.Parse(v);
                        if (a >= 1920 && a <= 2002)
                            n++;
                    }
                    else if (k == "iyr")
                    {
                        int a = int.Parse(v);
                        if (a >= 2010 && a <= 2020)
                            n++;
                    }
                    else if (k == "eyr")
                    {
                        int a = int.Parse(v);
                        if (a >= 2020 && a <= 2030)
                            n++;
                    }
                    else if (k == "hgt")
                    {
                        int a = 0;
                        if (v.Length > 2)
                            a = int.Parse(v.Remove(v.Length - 2));
                        if (v.EndsWith("cm") && a >= 150 && a <= 193)
                            n++;
                        else if (v.EndsWith("in") && a >= 59 && a <= 76)
                            n++;
                    }
                    else if (k == "hcl")
                    {
                        if (v.Length == 7 && v[0] == '#')
                        {
                            string hex = new string(v.Where(Uri.IsHexDigit).ToArray());
                            if (hex.Length == 6)
                                n++;
                        }
                    }
                    else if (k == "ecl")
                    {
                        if (v == "amb" || v == "blu" || v == "brn" || v == "gry" || v == "grn" || v == "hzl" || v == "oth")
                            n++;
                    }
                    else if (k == "pid")
                    {
                        string dec = new string(v.Where(char.IsDigit).ToArray());
                        if (dec.Length == 9)
                            n++;
                    }
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
