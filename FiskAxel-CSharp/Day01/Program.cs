using System;
using System.IO;

namespace FiskAxel___Day_1
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInputStrings = File.ReadAllLines("../../../puzzleInput1.txt");
            
            int[] puzzleInput = new int[puzzleInputStrings.Length];
            for (int i = 0; i < puzzleInputStrings.Length; i++)
            {
                puzzleInput[i] = int.Parse(puzzleInputStrings[i]);
            }

            ////
            //// PART 1
            ////

            for (int i = 0; i < puzzleInput.Length; i++)
            {
                bool dataFound = false;
                int data1 = puzzleInput[i];
                for (int j = i + 1; j < puzzleInput.Length; j++)
                {
                    int data2 = puzzleInput[j];
                    if (data1 + data2 == 2020)
                    {
                        Console.WriteLine($"{data1} + {data2} = {data1 + data2}");
                        Console.WriteLine($"{data1} * {data2} = {data1 * data2}");
                        dataFound = true;
                    }
                    if (dataFound) { break; }
                }
                if (dataFound) { break; }
            }


            ///
            /// PART 2
            ///

            for (int i = 0; i < puzzleInput.Length; i++)
            {
                bool dataFound = false;
                int data1 = puzzleInput[i];
                for (int j = 0; j < puzzleInput.Length; j++)
                {
                    int data2 = puzzleInput[j];
                    if (data1 + data2 <= 2020)
                    {
                        for (int k = 0; k < puzzleInput.Length; k++)
                        {
                            int data3 = puzzleInput[k];
                            if (data1 + data2 + data3 == 2020)
                            {
                                Console.WriteLine($"{data1} + {data2} + {data3} = {data1 + data2 + data3}");
                                Console.WriteLine($"{data1} * {data2} * {data3} = {data1 * data2 * data3}");
                                dataFound = true;
                            }
                        }
                        if (dataFound) { break; }
                    }
                    if (dataFound) { break; }
                }
                if (dataFound) { break; }
            }
        }
    }
}
