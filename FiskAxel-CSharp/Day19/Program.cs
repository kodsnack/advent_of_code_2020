using System;
using System.IO;

namespace Day19
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput19.txt");

            int length = Array.IndexOf(puzzleInput, "");
            string[] rules = new string[length];
            for (int i = 0; i < length; i++)
            {
                string[] parse = puzzleInput[i].Split(": ");
                int index = int.Parse(parse[0]);
                rules[index] = parse[1];
            }

            int length2 = puzzleInput.Length - (length + 1);
            string[] messages = new string[length2];
            for (int i = 0; i < length2; i++)
            {
                messages[i] = puzzleInput[length + 1 + i];
            }

            ////
            //// PART 1
            //// 

            int result = 0;
            foreach (string message in messages)
            {
                string[] messageAndCurrentIndex = new string[3];
                messageAndCurrentIndex[0] = message;
                messageAndCurrentIndex[1] = "0";
                messageAndCurrentIndex[2] = "first";
                
                if (validMessage(messageAndCurrentIndex, rules[0], rules) != "-1")
                {
                    result++;
                }
            }

            Console.Write("Part 1: ");
            Console.WriteLine(result);


            //// 
            //// PART 2
            ////

            rules[8] = "42 | 42 8";
            rules[11] = "42 31 | 42 11 31";

            result = 0;
            foreach (string message in messages)
            {
                string[] messageAndCurrentIndex = new string[3];
                messageAndCurrentIndex[0] = message;
                messageAndCurrentIndex[1] = "0";
                messageAndCurrentIndex[2] = "first";

                if (validMessage(messageAndCurrentIndex, rules[0], rules) != "-1")
                {
                    result++;
                }
            }

            Console.Write("Part 2: ");
            Console.WriteLine(result);
        }

        static string validMessage(string[] message, string rule, string[] rules)
        {
            bool firstRekLvl = false;
            if (message[2] == "first")
            {
                firstRekLvl = true;
                message[2] = "notFirst";
            }

            string[] parsedRule = rule.Split(' ');
            for (int i = 0; i < parsedRule.Length; i++)
            {
                int mi = int.Parse(message[1]);
                if (message[1] == "-1")
                {
                    return message[1];
                }
                
                int index = int.Parse(parsedRule[i]);
                if (mi >= message[0].Length)
                {
                    return "-1";
                }

                string[] copy = new string[3];
                copy[0] = message[0];
                copy[1] = message[1];
                copy[2] = message[2];
                if (rules[index].Contains("\""))
                {
                    if ($"\"{message[0][mi]}\"" != rules[index])
                    {
                        return "-1";
                    }
                    mi++;
                    message[1] = mi.ToString();
                }
                else if (rules[index].Contains("|"))
                {                    
                    string[] newRule = rules[index].Split(" | ");
                    string left = validMessage(copy, newRule[0], rules);
                    copy[1] = message[1];
                    string right = validMessage(copy, newRule[1], rules);
                    if (left != "-1" && right != "-1" && left != right)
                    {
                           
                    }
                    if (left != "-1")
                    {
                        message[1] = left;
                    }
                    else if (right != "-1")
                    {
                        message[1] = right;                        
                    }
                    else
                    {
                        return "-1";
                    }
                }
                else
                {
                    message[1] = validMessage(copy, rules[index], rules);
                }
            }

            if ((firstRekLvl && int.Parse(message[1]) != message[0].Length) || int.Parse(message[1]) > message[0].Length)
            {
                return "-1";
            }

            return message[1];
        }
    }
}
