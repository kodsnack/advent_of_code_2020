using System;
using System.Collections.Generic;
using System.IO;

namespace Day04
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput4.txt");

            List<string> passports = new List<string>();
            List<string> lines = new List<string>();

            for (int i = 0; i < puzzleInput.Length; i++)
            {
                string line = puzzleInput[i];
                if (line == "")
                {
                    AddToPassport(passports, lines);
                }
                else
                {
                    lines.Add(line);
                }
            } AddToPassport(passports, lines); //Last passport

            ////
            //// PART 1
            ////
         
            int validPassports = 0;
            foreach (string passport in passports)
            {
                if (isValid(passport))
                {
                    validPassports++;
                }
            }
            Console.WriteLine($"Part 1 - Valid passports: {validPassports}");
            
            ////
            //// PART 2
            ////

            validPassports = 0;
            foreach (string passport in passports)
            {
                if (isValid2(passport))
                {
                    validPassports++;
                }
            }
            Console.WriteLine($"Part 2 - Valid passports: {validPassports}");
        }
        
        static void AddToPassport(List<string> passports, List<string> lines) 
        { 
            passports.Add(lines[0]);
            for (int j = 1; j < lines.Count; j++)
            {
                passports[passports.Count - 1] += " ";
                passports[passports.Count - 1] += lines[j];
            }
            passports[passports.Count - 1] += " ";
            lines.Clear();
        }

        static bool isValid(string passport)
        {
            if(passport.Contains("byr:") &&
               passport.Contains("iyr:") &&
               passport.Contains("eyr:") &&
               passport.Contains("hgt:") &&
               passport.Contains("hcl:") &&
               passport.Contains("ecl:") &&
               passport.Contains("pid:"))
            {
                return true;
            }
            else { return false; }
        }
        
        static bool isValid2(string passport)
        {
            if (!isValid(passport))
            {
                return false;
            }
            if(byrValid(passport) &&
               iyrValid(passport) &&
               eyrValid(passport) &&
               hgtValid(passport) &&
               hclValid(passport) &&
               eclValid(passport) &&
               pidValid(passport))
            {
                return true;
            }
            else { return false; }
        }
        static bool byrValid(string passport)
        {
            int start = passport.IndexOf("byr:") + 4;
            string byrStr = passport.Substring(start, 5);
            if (byrStr[4] == ' ')
            {
                int byr = int.Parse(byrStr.Substring(0, 4));
                if(1920 <= byr && byr <= 2002)
                {
                    return true;
                }
            }
            return false;
        }
        static bool iyrValid(string passport)
        {
            int start = passport.IndexOf("iyr:") + 4;
            string iyrStr = passport.Substring(start, 5);
            if (iyrStr[4] == ' ')
            {
                int iyr = int.Parse(iyrStr.Substring(0, 4));
                if (2010 <= iyr && iyr <= 2020)
                {
                    return true;
                }
            }
            return false;
        }
        static bool eyrValid(string passport)
        {
            int start = passport.IndexOf("eyr:") + 4;
            string eyrStr = passport.Substring(start, 5);
            if (eyrStr[4] == ' ')
            {
                 int eyr = int.Parse(eyrStr.Substring(0, 4));
                if (2020 <= eyr && eyr <= 2030)
                {
                    return true;
                }
            }
            return false;
        }
        static bool hgtValid(string passport)
        {
            int start = passport.IndexOf("hgt:") + 4;
            string hgt = passport.Substring(start, 5);
            if(hgt.Contains("cm") && hgt[3] == 'c')
            {
                int height = int.Parse(hgt.Substring(0, 3));
                if (150 <= height && height <= 193)
                {
                    return true;
                }
            }
            else if(hgt.Contains("in") && hgt[2] == 'i')
            {
                int height = int.Parse(hgt.Substring(0, 2));
                if (59 <= height && height <= 76)
                {
                    return true;
                }
            }
            return true;
        }
        static bool hclValid(string passport)
        {
            int start = passport.IndexOf("hcl:") + 4;
            string hclStr1 = passport.Substring(start, passport.Length - start);
            int end = passport.IndexOf(" ");
            string hclStr2 = hclStr1.Substring(0, 8);
            if (hclStr2[7] == ' ' && hclStr2[0] == '#')
            {
                string hcl = hclStr2.Substring(1, 6);
                if (isHexadecimal(hcl))
                {
                    return true;
                }
            }
            return false;
        }
        static bool eclValid(string passport)
        {
            int start = passport.IndexOf("ecl:") + 4;
            string eclStr = passport.Substring(start, 4);
            if (eclStr[3] == ' ')
            {
                string ecl = eclStr.Substring(0, 3);
                if (String.Equals(ecl, "amb") ||
                    String.Equals(ecl, "blu") ||
                    String.Equals(ecl, "brn") ||
                    String.Equals(ecl, "gry") ||
                    String.Equals(ecl, "grn") ||
                    String.Equals(ecl, "hzl") ||
                    String.Equals(ecl, "oth"))
                {
                    return true;
                }
            }
            return false;
        }
        static bool pidValid(string passport)
        {
            int start = passport.IndexOf("pid:") + 4;
            if (passport.Substring(start).Length < 10) 
            {
                return false;
            }
            string pidStr = passport.Substring(start, 10);
            if (pidStr[9] == ' ')
            {
                string pid = pidStr.Substring(0, 9);
                if (int.TryParse(pidStr, out int a))
                {
                    return true;
                }
            }
            return false;
        }

        static bool isHexadecimal(string num)
        {
            for (int i = 0; i < num.Length; i++)
            {
                if ('0' <= num[i] && num[i] <= '9' ||
                    'a' <= num[i] && num[i] <= 'f') 
                {}
                else 
                { 
                    return false; 
                }
            }
            return true;
        }
    }
}
