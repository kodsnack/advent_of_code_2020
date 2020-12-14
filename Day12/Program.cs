using System;
using System.IO;

namespace Day12
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput12.txt");

            int southNorth = 0; // S- N+ W- E+
            int westEast = 0;

            int direction = 90; // N0 E90 S180 W270

            ////
            //// PART 1
            //// 

            foreach (string input in puzzleInput)
            {
                int number = int.Parse(input.Substring(1));

                if (input[0] == 'S' || input[0] == 'F' && direction == 180)
                {
                    southNorth -= number;
                }
                else if (input[0] == 'N' || input[0] == 'F' && direction == 0)
                {
                    southNorth += number;
                }
                else if (input[0] == 'W' || input[0] == 'F' && direction == 270)
                {
                    westEast -= number;
                }
                else if (input[0] == 'E' || input[0] == 'F' && direction == 90)
                {
                    westEast += number;
                }

                else if (input[0] == 'L')
                {
                    direction -= number;
                    if (direction < 0)
                    {
                        direction += 360;
                    }
                }
                else if (input[0] == 'R')
                {
                    direction += number;
                    direction %= 360;
                }
            }

            Console.WriteLine("PART 1");
            Console.WriteLine($"South/North: {southNorth}");
            Console.WriteLine($"West/East: {westEast}");


            ////
            //// PART 2 
            ////

            southNorth = 0;
            westEast = 0;
            int waypointSN = 1;
            int waypointWE = 10;

            foreach (string input in puzzleInput)
            {
                int number = int.Parse(input.Substring(1));

                if (input[0] == 'F')
                {
                    southNorth += waypointSN * number;
                    westEast += waypointWE * number;
                }

                else if (input[0] == 'S')
                {
                    waypointSN -= number;
                }
                else if (input[0] == 'N')
                {
                    waypointSN += number;
                }
                else if (input[0] == 'W')
                {
                    waypointWE -= number;
                }
                else if (input[0] == 'E')
                {
                    waypointWE += number;
                }

                else if (input == "R90" || input == "L270")
                {
                    int temp = waypointSN;
                    waypointSN = (-waypointWE);
                    waypointWE = temp;
                }
                else if (input == "R180" || input == "L180")
                {
                    waypointSN *= -1;
                    waypointWE *= -1;
                }
                else if (input == "R270" || input == "L90")
                {
                    int temp = waypointWE;
                    waypointWE = (-waypointSN);
                    waypointSN = temp;
                }
            }

            Console.WriteLine("\nPART 2");
            Console.WriteLine($"South/North: {southNorth}");
            Console.WriteLine($"West/East: {westEast}");
        }
    }
}
