using System;
using System.IO;

namespace Day10
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../test2.txt");
            int[] numbers = new int[puzzleInput.Length];
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                numbers[i] = int.Parse(puzzleInput[i]);
            }
            Array.Sort(numbers, 0, numbers.Length);

            ////
            //// PART 1
            ////

            int ones = 1;
            int threes = 1;
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

            int[] numbers2 = new int[numbers.Length + 2];
            numbers2[0] = 0;
            for (int i = 0; i < numbers.Length; i++)
            {
                numbers2[i + 1] = numbers[i];
            }
            numbers2[numbers2.Length - 1] = numbers[numbers.Length - 1] + 3;

            double power = numbers2.Length - shortestSequenceLength(numbers2, 0);
            double total = Math.Pow(2, power);
            Console.WriteLine("Part 2");
            Console.WriteLine(total);
        }

        static int shortestSequenceLength(int[] numbers, int i)
        {
            int shortest = 0;

            if (i == numbers.Length - 1)
            {
                return 1;
            }
            
            if (i < numbers.Length - 3 && numbers[i] + 3 == numbers[i + 3])
            {
                shortest += shortestSequenceLength(numbers, i + 3);
                shortest++;
            }

            else if (i < numbers.Length - 2 && numbers[i] + 2 == numbers[i + 2] ||
                     i < numbers.Length - 2 && numbers[i] + 3 == numbers[i + 2])
            {
                shortest += shortestSequenceLength(numbers, i + 2);
                shortest++;
            }

            else if (numbers[i] + 1 == numbers[i + 1] ||
                     numbers[i] + 2 == numbers[i + 1] ||
                     numbers[i] + 3 == numbers[i + 1])
            {
                shortest += shortestSequenceLength(numbers, i + 1);
                shortest++;
            }

            return shortest;
        }



            //Working but too inefficient
            static int numberOfValidOrders(int[] numbers, int i)
        {
            int sum = 0;

            if (i == numbers.Length - 1)
            {
                return 1;
            }
 
            if (numbers[i] + 1 == numbers[i + 1] || 
                numbers[i] + 2 == numbers[i + 1] || 
                numbers[i] + 3 == numbers[i + 1])
            {
                sum += numberOfValidOrders(numbers, i + 1);
            }

            if (i < numbers.Length - 2)
            {
                if (numbers[i] + 2 == numbers[i + 2] ||
                    numbers[i] + 3 == numbers[i + 2])
                {
                    sum += numberOfValidOrders(numbers, i + 2);
                }
            }
            if (i < numbers.Length - 3)
            {         
                if (numbers[i] + 3 == numbers[i + 3])
                {
                    sum += numberOfValidOrders(numbers, i + 3);
                }
            }

            return sum;
        }
    }
}
