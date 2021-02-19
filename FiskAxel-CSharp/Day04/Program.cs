using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Day04
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput4.txt");

            List<string> passports = new List<string>();
            passports.Add("");
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                if (puzzleInput[i] == "")
                {
                    passports.Add("");
                    continue;
                }  
                passports[passports.Count - 1] += puzzleInput[i];
                passports[passports.Count - 1].Trim();
                passports[passports.Count - 1] += ' ';
            }

            int result = 0;
            List<string> validPassports1 = new List<string>();
            for (int i = 0; i < passports.Count; i++)
            {
                if (passports[i].Contains("byr:") &&
                    passports[i].Contains("iyr:") &&
                    passports[i].Contains("eyr:") &&
                    passports[i].Contains("hgt:") &&
                    passports[i].Contains("hcl:") &&
                    passports[i].Contains("ecl:") &&
                    passports[i].Contains("pid:"))
                {
                    result++;
                    validPassports1.Add(passports[i]);
                }
            }

            Console.WriteLine("Part 1: ");
            Console.WriteLine(result);

            result = 0;
            for (int i = 0; i < validPassports1.Count; i++)
            {
                if (valid2(validPassports1[i]))
                {
                    result++;
                }
            }

            Console.WriteLine("Part 2: ");
            Console.WriteLine(result);
        }

        static bool valid2(string pass) 
        {
            string byr = getString(pass, pass.IndexOf("byr:"));
            if (byr.Length != 4 || int.Parse(byr) < 1920 || 2002 < int.Parse(byr))
            {
                return false;
            }

            string iyr = getString(pass, pass.IndexOf("iyr:"));
            if (iyr.Length != 4 || int.Parse(iyr) < 2010 || 2020 < int.Parse(iyr))
            {
                return false;
            }

            string eyr = getString(pass, pass.IndexOf("eyr:"));
            if (eyr.Length != 4 || int.Parse(eyr) < 2020 || 2030 < int.Parse(eyr))
            {
                return false;
            }

            string hgt = getString(pass, pass.IndexOf("hgt:"));
            if (hgt.Contains("cm"))
            {
                int heigth = int.Parse(hgt.Substring(0, hgt.IndexOf('c')));
                if (heigth < 150 || 193 < heigth)
                {
                    return false;
                }   
            }
            else if (hgt.Contains("in"))
            {
                int heigth = int.Parse(hgt.Substring(0, 2));
                if (heigth < 59 || 76 < heigth)
                {
                    return false;
                }
            }
            else { return false; }

            string hcl = getString(pass, pass.IndexOf("hcl:"));
            Regex reg = new Regex("[0-9a-f]");
            if (hcl[0] != '#' || hcl.Length != 7 || reg.IsMatch(hcl.Substring(1)) == false)
            {
                return false;
            }

            string ecl = getString(pass, pass.IndexOf("ecl:"));
            if (ecl != "amb" && ecl != "blu" && ecl != "brn" && 
                ecl != "gry" && ecl != "grn" && ecl != "hzl" && ecl != "oth")
            {
                return false;
            }

            int apa = 0;
            string pid = getString(pass, pass.IndexOf("pid:"));
            if (pid.Length != 9 || int.TryParse(pid, out apa) == false)
            {
                return false;
            }

            return true;
        }

        static string getString(string input, int start)
        {
            string output = input.Substring(start + 4);
            output = output.Substring(0, output.IndexOf(" "));
            return output;
        }
    }
}
