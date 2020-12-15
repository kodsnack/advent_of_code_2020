using System;
using System.IO;

namespace Day08
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput8.txt");
            
            ////
            //// PART 1
            ////
            
            int acc = 0;
            int[] runLines = new int[puzzleInput.Length];

            for (int i = 0; i < puzzleInput.Length; i++)
            {
                char instruction = puzzleInput[i][0];
                char operand = puzzleInput[i][4];
                int num = int.Parse(puzzleInput[i].Substring(5));
                
                if (runLines[i] == 0)
                {
                    runLines[i] = 1;
                }
                else 
                {
                    break;
                }
                
                if (instruction == 'a')
                {
                    if (operand == '+')
                    {
                        acc += num;
                    }
                    if (operand == '-')
                    {
                        acc -= num;
                    }
                }
                else if (instruction == 'j')
                {
                    if (operand == '+')
                    {
                        i += num - 1;
                    }
                    if (operand == '-')
                    {
                        i -= num + 1;
                    }
                }
            }

            Console.Write("Part 1: ");
            Console.WriteLine(acc);


            ////
            //// PART 2
            ////           
            
            int change = 0;       
            bool terminated = false;
            while (!terminated)
            {
                acc = 0;
                int[] runLines2 = new int[puzzleInput.Length];
                for (int i = 0; i < puzzleInput.Length; i++)
                {
                    char instruction = puzzleInput[i][0];
                    char operand = puzzleInput[i][4];
                    int num = int.Parse(puzzleInput[i].Substring(5));

                    if (instruction != 'a' && i == change)
                    {
                        if (instruction == 'n')
                        {
                            instruction = 'j';
                        }
                        else
                        {
                            instruction = 'n';
                        }
                    }

                    if (runLines2[i] == 0)
                    {
                        runLines2[i] = 1;
                    }
                    else
                    {
                        break;
                    }

                    if (instruction == 'a')
                    {
                        if (operand == '+')
                        {
                            acc += num;
                        }
                        if (operand == '-')
                        {
                            acc -= num;
                        }
                    }
                    else if (instruction == 'j')
                    {
                        if (operand == '+')
                        {
                            i += num - 1;
                        }
                        if (operand == '-')
                        {
                            i -= num + 1;
                        }
                    }

                    if (i > runLines2.Length || runLines2[runLines2.Length - 1] == 1)
                    {
                        terminated = true;
                        break;
                    }
                }
                change++;
            }

            Console.Write("Part 2: ");
            Console.WriteLine(acc);
        }
    }
}