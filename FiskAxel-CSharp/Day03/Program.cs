using System;
using System.IO;

namespace Day03
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput3.txt");

            ////
            //// PART 1
            ////

            int treeEncounters = airplaneTravel(puzzleInput, 0, 0, 3, 1);
            Console.WriteLine($"Tree encounters: {treeEncounters}\n");

            ////
            //// PART 2
            ////

            int treeEncounters1 = airplaneTravel(puzzleInput, 0, 0, 1, 1);
            Console.WriteLine($"Tree encounters (Right 1, down 1): {treeEncounters1}");
            int treeEncounters2 = airplaneTravel(puzzleInput, 0, 0, 3, 1);
            Console.WriteLine($"Tree encounters (Right 3, down 1): {treeEncounters2}");
            int treeEncounters3 = airplaneTravel(puzzleInput, 0, 0, 5, 1);
            Console.WriteLine($"Tree encounters (Right 5, down 1): {treeEncounters3}");
            int treeEncounters4 = airplaneTravel(puzzleInput, 0, 0, 7, 1);
            Console.WriteLine($"Tree encounters (Right 7, down 1): {treeEncounters4}");
            int treeEncounters5 = airplaneTravel(puzzleInput, 0, 0, 1, 2);
            Console.WriteLine($"Tree encounters (Right 1, down 2): {treeEncounters5}");

            long multiplied = (long)treeEncounters1 * treeEncounters2 * treeEncounters3 * treeEncounters4 * treeEncounters5;
            Console.WriteLine($"Multiplied: {multiplied}");

        }
        static int airplaneTravel(string[] input, int x, int y, int right, int down)
        {
            if (y >= input.Length)
            {
                return 0;
            }
            int counter = 0;
            if (input[y][x] == '#')
            {
                counter++;
            }

            x += right;
            x = x % input[0].Length;
            y += down;
            counter += airplaneTravel(input, x, y, right, down);
            return counter;
        }
    }
}
