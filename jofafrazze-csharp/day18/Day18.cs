using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

using AdventOfCode;
//using Position = AdventOfCode.GenericPosition2D<int>;

namespace day18
{
    public class Day18
    {
        readonly static string nsname = typeof(Day18).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        static long Calc(string s, int p1, int p2)
        {
            int n = p1;
            long acc = 0;
            char op = ' ';
            while (n < p2)
            {
                char c = s[n];
                bool dig = Char.IsDigit(c);
                if (dig || (c == '('))
                {
                    long a = 0;
                    if (dig)
                        a = c - '0';
                    else
                    {
                        int depth = 0;
                        bool done = false;
                        int i = n;
                        for (; i < p2 && !done; i++)
                        {
                            if (s[i] == '(')
                                depth++;
                            else if (s[i] == ')')
                                depth--;
                            if (depth == 0)
                                done = true;
                        }
                        a = Calc(s, n + 1, i - 1);
                        n = i;
                    }
                    if (op == '+')
                        acc += a;
                    else if (op == '*')
                        acc *= a;
                    else
                        acc = a;
                }
                else if (c == '+')
                    op = c;
                else if (c == '*')
                    op = c;
                else if (c == ' ')
                    ;
                else
                    throw new InvalidProgramException();
                n++;

            }
            return acc;
        }

        static Object PartA()
        {
            var input = ReadIndata.Strings(inputPath);
            long ans = 0;
            foreach (string s in input)
            {
                ans += Calc(s, 0, s.Length);
            }
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static long CalcB(string s, int p1, int p2)
        {
            // Any * on our level?
            int depth2 = 0;
            bool done2 = false;
            int w = p1;
            for (; w < p2 && !done2; w++)
            {
                if (s[w] == '(')
                    depth2++;
                else if (s[w] == ')')
                    depth2--;
                if (depth2 == 0 && s[w] == '*')
                    done2 = true;
            }
            if (done2)
            {
                return CalcB(s, p1, w - 1) * CalcB(s, w, p2);
            }
            // No * on our level
            int n = p1;
            long acc = 0;
            char op = ' ';
            while (n < p2)
            {
                char c = s[n];
                bool dig = Char.IsDigit(c);
                if (dig || (c == '('))
                {
                    long a = 0;
                    if (dig)
                        a = c - '0';
                    else
                    {
                        int depth = 0;
                        bool done = false;
                        int i = n;
                        for (; i < p2 && !done; i++)
                        {
                            if (s[i] == '(')
                                depth++;
                            else if (s[i] == ')')
                                depth--;
                            if (depth == 0)
                                done = true;
                        }
                        a = CalcB(s, n + 1, i - 1);
                        n = i;
                    }
                    if (op == '+')
                        acc += a;
                    else if (op == '*')
                        acc *= a;
                    else
                        acc = a;
                }
                else if (c == '+')
                    op = c;
                else if (c == '*')
                    op = c;
                else if (c == ' ')
                    ;
                else
                    throw new InvalidProgramException();
                n++;

            }
            return acc;
        }

        static Object PartB()
        {
            var input = ReadIndata.Strings(inputPath);
            long ans = 0;
            foreach (string s in input)
            {
                ans += CalcB(s, 0, s.Length);
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
            long a = 7293529867931;
            long b = 60807587180737;
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
