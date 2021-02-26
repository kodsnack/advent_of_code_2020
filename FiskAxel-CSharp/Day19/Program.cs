using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace Day19
{
    class Program
    {
        static void Main(string[] args)
        {
            //// INPUT PARSING

            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput19.txt");

            int len = Array.IndexOf(puzzleInput, "");
            string[] rules = new string[len];
            for (int i = 0; i < len; i++)
            {
                string[] parse = puzzleInput[i].Split(": ");
                int index = int.Parse(parse[0]);
                rules[index] = parse[1];
            }

            int len2 = puzzleInput.Length - (len + 1);
            string[] messages = new string[len2];
            for (int i = 0; i < len2; i++)
            {
                messages[i] = puzzleInput[len + 1 + i];
            }

            ////
            //// PART 1
            ////

            int result = 0;
            Regex pattern1 = new Regex("^" + CreatePattern(rules, rules[0], 0, 0) + "$");
            foreach (var message in messages)
            {
                if (pattern1.IsMatch(message))
                {
                    result++;
                }
            }

            Console.WriteLine("Part 1: ");
            Console.WriteLine(result);
            
            ////
            //// PART 2
            ////

            rules[8] = "42 | 42 8";
            rules[11] = "42 31 | 42 11 31";

            result = 0;
            Regex pattern2 = new Regex("^" + CreatePattern(rules, rules[0], 0, 0) + "$");
            foreach (var message in messages)
            {
                if (pattern2.IsMatch(message))
                {
                    result++;
                }
            }

            Console.WriteLine("Part 2: ");
            Console.WriteLine(result);
        }

        static string CreatePattern(string[] rules, string rule, int len8, int len11)
        {
            if (rule == rules[8])
            {
                len8++;
                if (len8 > 10)
                {
                    rule = "42";
                } 
            }
            else if (rule == rules[11])
            {
                len11++;
                if (len11 > 10)
                {
                    rule = "42 31";
                }  
            }
            else if (rule[0] == '\"')
            {
                return rule.Substring(1,1);
            }

            string regPattern = "(";
            string[] parsed = rule.Split(" ");
            foreach (var item in parsed)
            {
                if (item == "|")
                {
                    regPattern += '|';
                }
                else
                {
                    regPattern += CreatePattern(rules, rules[int.Parse(item)], len8, len11);
                }
            }
            regPattern += ')';

            return regPattern;
        }
    } 
}