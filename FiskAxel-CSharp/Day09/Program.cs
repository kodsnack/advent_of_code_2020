using System;
using System.IO;

namespace Day09
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput9.txt");
            double[] numbers = new double[puzzleInput.Length];
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                numbers[i] = double.Parse(puzzleInput[i]);
            }

            ////
            //// PART 1
            ////

            double theNumber = 0;
            for (int i = 25; i < numbers.Length; i++)
            {
                bool found = true;
                for (int j = i - 25; j < i; j++)
                {
                    for (int k = i - 25; k < i; k++)
                    {
                        if (j != k)
                        {
                            if (numbers[i] == numbers[j] + numbers[k])
                            {
                                found = false;                                
                            }
                        }
                    }
                }
                if (found)
                {
                    theNumber = numbers[i];
                    break;
                }
            }

            Console.Write("Part 1: ");
            Console.WriteLine(theNumber);


            ////
            //// PART 2
            ////

            double theNumber2 = -1;
            double sum = 0;
            double lowest = double.MaxValue;
            double highest = -1;
            for (int i = 0; i < numbers.Length; i++)
            {
                sum = 0;
                int index = 0;
                while (sum < theNumber || i + index > numbers.Length - 1)
                {
                    sum += numbers[i + index];
                    index++;
                }
                if (sum == theNumber)
                {
                    for (int j = i; j < i + index; j++)
                    {
                        if (numbers[j] < lowest)
                        {
                            lowest = numbers[j];
                        }
                        if (numbers[j] > highest)
                        {
                            highest = numbers[j];
                        }
                    }
                    break;
                }
            }

            theNumber2 = lowest + highest;
            Console.Write("Part 2: ");
            Console.WriteLine(theNumber2);
        }
    }
}