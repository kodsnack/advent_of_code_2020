using System;
using System.Collections.Generic;
using System.IO;

namespace Day18
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput18.txt");

            //// 
            //// PART 1
            //// 
            ulong sumOfAll = 0;
            foreach (string matteTal in puzzleInput)
            {
                sumOfAll += solve1(matteTal);
            }

            Console.Write("Part 1: ");
            Console.WriteLine(sumOfAll);


            ////
            //// PART 2
            ////

            sumOfAll = 0;
            foreach (string matteTal in puzzleInput)
            {
                sumOfAll += solve2(matteTal);
            }

            Console.Write("Part 2: ");
            Console.WriteLine(sumOfAll);
            Console.ReadKey();
        }
        
        static ulong solve1(string input)
        {
            ulong result = 0;
            char ope = '+';

            for (int i = 0; i < input.Length; i++)
            {
                if (input[i] == ' ') {}
                else if (input[i] == '+')
                {
                    ope = '+';
                }                 
                else if (input[i] == '*')
                {
                    ope = '*';
                }    
                else if (input[i] == '(')
                {
                    string newInput = input.Substring(i + 1);
                    if (ope == '+')
                    {
                        result += solve1(newInput);
                    }
                    else if (ope == '*')
                    {
                        result *= solve1(newInput);
                    }

                    int openParentsIn = 0;
                    for (int j = i + 1; j < input.Length; j++)
                    {
                        if (input[j] == '(')
                        {
                            openParentsIn++;
                        }
                        else if (input[j] == ')')
                        {
                            if (openParentsIn > 0)
                            {
                                openParentsIn--;
                            }
                            else
                            {
                                i = j;
                                break;
                            }
                        }
                    }
                }       
                else if (input[i] == ')')
                {    
                    break;   
                }
                else
                {
                    string num = "";
                    num += input[i];
                    if (ope == '+')
                    {
                        result += ulong.Parse(num);
                    }
                    else if( ope == '*')
                    {
                        result *= ulong.Parse(num);
                    }
                }
            }   
            
            return result;
        }

        static ulong solve2(string input)
        {
            List<string> inputList = new List<string>();
            for (int i = 0; i < input.Length; i++)
            {
                if (input[i] == ' ') { }
                else if (input[i] == '(')
                {
                    string newInput = input.Substring(i + 1);
                    string parse = "";
                    parse += solve2(newInput);
                    inputList.Add(parse);

                    int openParentsIn = 0;
                    for (int j = i + 1; j < input.Length; j++)
                    {
                        if (input[j] == '(')
                        {
                            openParentsIn++;
                        }
                        else if (input[j] == ')')
                        {
                            if (openParentsIn > 0)
                            {
                                openParentsIn--;
                            }
                            else
                            {
                                i = j;
                                break;
                            }
                        }
                    }
                }
                else if (input[i] == ')')
                {
                    break;
                }
                else
                {
                    string parse = "";
                    parse += input[i];
                    inputList.Add(parse);
                }
            }

            for (int i = 0; i < inputList.Count; i++)
            {
                if (inputList[i] == "+")
                {
                    ulong num1 = ulong.Parse(inputList[i - 1]);
                    ulong num2 = ulong.Parse(inputList[i + 1]);
                    inputList[i - 1] = "";
                    inputList[i - 1] += (num1 + num2);
                    inputList.RemoveAt(i + 1);
                    inputList.RemoveAt(i);
                    i--;
                }
            }

            for (int i = 0; i < inputList.Count; i++)
            {
                if (inputList[i] == "*")
                {
                    ulong num1 = ulong.Parse(inputList[i - 1]);
                    ulong num2 = ulong.Parse(inputList[i + 1]);
                    inputList[i - 1] = "";
                    inputList[i - 1] += (num1 * num2);
                    inputList.RemoveAt(i + 1);
                    inputList.RemoveAt(i);
                    i--;
                }
            }

            ulong num = ulong.Parse(inputList[0]);
            ulong result = num;

            return result;
        }
    } 
}