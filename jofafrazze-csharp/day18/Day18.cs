using System;
using System.IO;
using AdventOfCode;

namespace day18
{
    public class Day18
    {
        readonly static string nsname = typeof(Day18).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static int ClosingParenthesisIdx(string s, int idx)
        {
            int d = 0;
            int i = idx;
            for (; i < s.Length; i++)
            {
                if (s[i] == '(')
                    d++;
                else if (s[i] == ')')
                    d--;
                else if (d == 0)
                    break;
            }
            return i - 1;
        }

        static long Calc(string s, bool advanced)
        {
            // Skip outmost parenthesise
            while (s[0] == '(' && ClosingParenthesisIdx(s, 0) == s.Length - 1)
                s = s.Substring(1, s.Length - 2);
            // Any * on our level?
            int d = 0;
            for (int i = 0; advanced && i < s.Length; i++)
            {
                if (s[i] == '(')
                    d++;
                else if (s[i] == ')')
                    d--;
                else if (s[i] == '*' && d == 0)
                    return Calc(s.Substring(0, i - 1), advanced) * Calc(s.Substring(i + 2), advanced);
            }
            // No * on our level
            long acc = 0;
            char op = ' ';
            for (int i = 0; i < s.Length; i++)
            {
                long a = 0;
                char c = s[i];
                if (c == ' ')
                    continue;
                if (Char.IsDigit(c) || (c == '('))
                {
                    if (Char.IsDigit(c))
                        a = c - '0';
                    else
                    {
                        int j = ClosingParenthesisIdx(s, i);
                        a = Calc(s.Substring(i + 1, j - (i + 1)), advanced);
                        i = j;
                    }
                    if (op == '+')
                        acc += a;
                    else if (op == '*')
                        acc *= a;
                    else
                        acc = a;
                }
                else if (c == '+' || c == '*')
                    op = c;
            }
            return acc;
        }

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            long ans = 0;
            foreach (string s in input)
                ans += Calc(s, false);
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            long ans = 0;
            foreach (string s in input)
                ans += Calc(s, true);
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
            long a = 7293529867931;
            long b = 60807587180737;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
