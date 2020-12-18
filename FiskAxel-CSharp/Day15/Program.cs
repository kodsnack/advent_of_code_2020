using System;
using System.Collections.Generic;
using System.IO;

namespace Day15
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput15.txt");
            string[] input = puzzleInput[0].Split(',');
            List<int> numbers = new List<int>();

            ////
            //// PART 1
            ////

            for (int i = 0; i < input.Length; i++)
            {
                numbers.Add(int.Parse(input[i]));
            }

            while (numbers.Count < 2020)
            {
                int nextNumber = 0;
                for (int i = numbers.Count - 2; i >= 0; i--)
                {
                    if (numbers[i] == numbers[numbers.Count - 1])
                    {
                        nextNumber = numbers.Count - (i + 1);
                        break;
                    }
                }
                numbers.Add(nextNumber);
            }
           
            Console.Write("Part 1:");
            Console.WriteLine(numbers[2019]);


            ////
            //// PART 2
            ////

            numbers.Clear();         
            for (int i = 0; i < input.Length; i++)
            {
                numbers.Add(int.Parse(input[i]));
                
            }
            int[] numbers2 = new int[30000000];
            for (int i = 0; i < input.Length - 1; i++)
            {
                numbers2[int.Parse(input[i])] = i + 1;
            }

            while (numbers.Count != 30000001)
            {
                int lastNumber = numbers[numbers.Count - 1];
                int nextNumber = 0;
                if (numbers2[lastNumber] != 0)
                {
                    nextNumber = numbers.Count - numbers2[lastNumber];
                }
                numbers2[lastNumber] = numbers.Count;
                numbers.Add(nextNumber);
            }

            int result = Array.IndexOf(numbers2, 30000000);
            Console.Write("Part 2:");
            Console.WriteLine(result);

        }
    }
}
