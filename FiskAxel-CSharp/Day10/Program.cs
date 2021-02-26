using System;
using System.IO;

namespace Day10
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput10.txt");
            int[] numbers = new int[puzzleInput.Length];
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                numbers[i] = int.Parse(puzzleInput[i]);
            }
            Array.Sort(numbers, 0, numbers.Length);

            ////
            //// PART 1
            ////

            int ones = 1; // From power outlet.
            int threes = 1; // To device.
            for (int i = 0; i < numbers.Length - 1; i++)
            {
                int dif = 0;
                dif = numbers[i + 1] - numbers[i];
                if (dif == 1)
                {
                    ones++;
                }
                else if (dif == 3)
                {
                    threes++;
                }
            }
            Console.WriteLine("Part 1");
            Console.WriteLine($"NumOfOnes: {ones} * NumOfThrees: {threes}. Equals: {ones * threes}\n");


            ////
            //// PART 2
            ////

            int[] numbers2 = new int[numbers.Length + 1];
            numbers2[0] = 0; // Outlet.
            for (int i = 0; i < numbers.Length; i++)
            {
                numbers2[i + 1] = numbers[i];
            }
            long[] ways = new long[numbers2.Length];
            ways[ways.Length - 1] = 1;
            for (int i = ways.Length - 2; i >= 0; i--)
            {
                if (numbers2[i] + 3 >= numbers2[i + 1])
                {
                    ways[i] += ways[i + 1];
                }
                if (i + 2 < numbers2.Length && numbers2[i] + 3 >= numbers2[i + 2])
                {
                    ways[i] += ways[i + 2];
                }
                if (i + 3 < numbers2.Length && numbers2[i] + 3 >= numbers2[i + 3])
                {
                    ways[i] += ways[i + 3];
                }
            }

            Console.WriteLine("Part 2");
            Console.WriteLine(ways[0]);
        }
    }
}
