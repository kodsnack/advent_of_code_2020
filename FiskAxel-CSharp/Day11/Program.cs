using System;
using System.IO;
using System.Linq;


namespace Day11
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput11.txt");

            int yLength = puzzleInput.Length;
            int xLength = puzzleInput[0].Length;
             
            
            ////
            //// PART 1
            ////

            while (true)
            {
                
                string[] temp = new string[yLength];
                for (int y = 0; y < yLength; y++)
                {
                    for (int x = 0; x < xLength; x++)
                    {
                        int filledAdjacentSeats = 0;
                        if (y - 1 >= 0 && x - 1 >= 0)
                        {
                            if (puzzleInput[y - 1][x - 1] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }
                        if (y - 1 >= 0)
                        {
                            if (puzzleInput[y - 1][x] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }
                        if (y - 1 >= 0 && x + 1 < xLength)
                        {
                            if (puzzleInput[y - 1][x + 1] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }
                        if (x - 1 >= 0)
                        {
                            if (puzzleInput[y][x - 1] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }
                        if (x + 1 < xLength)
                        {
                            if (puzzleInput[y][x + 1] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }
                        if (y + 1 < yLength && x - 1 >= 0)
                        {
                            if (puzzleInput[y + 1][x - 1] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }
                        if (y + 1 < yLength)
                        {
                            if (puzzleInput[y + 1][x] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }
                        if (y + 1 < yLength && x + 1 < xLength)
                        {
                            if (puzzleInput[y + 1][x + 1] == '#')
                            {
                                filledAdjacentSeats++;
                            }
                        }


                        if (puzzleInput[y][x] == '.')
                        {
                            temp[y] += '.';
                        }

                        else if (puzzleInput[y][x] == '#' && filledAdjacentSeats >= 4)
                        {
                            temp[y] += 'L';
                        }
                        else if (puzzleInput[y][x] == '#')
                        {
                            temp[y] += '#';
                        }

                        else if (puzzleInput[y][x] == 'L' && filledAdjacentSeats == 0)
                        {
                            temp[y] += '#';
                        }
                        else if (puzzleInput[y][x] == 'L')
                        {
                            temp[y] += 'L';
                        }
                    }
                }
                if (puzzleInput.SequenceEqual(temp))
                {
                    break;
                }
                puzzleInput = temp;
            }

            int total = countOccupiedSeats(puzzleInput);
            Console.WriteLine($"Part 1 Occupied seats: {total}");


            ////
            //// PART 2
            ////
            
            puzzleInput = File.ReadAllLines("../../../puzzleInput11.txt");
            while (true)
            {
                string[] temp = new string[yLength];
                for (int y = 0; y < yLength; y++)
                {
                    for (int x = 0; x < xLength; x++)
                    {
                        int visibleOccupiedSeats = UpLeft(puzzleInput, y, x) + Up(puzzleInput, y, x) + 
                                                   UpRight(puzzleInput, y, x) + Left(puzzleInput, y, x) + Right(puzzleInput, y, x) + 
                                                   DownLeft(puzzleInput, y, x) + Down(puzzleInput, y, x) + DownRight(puzzleInput, y, x);
                    
                        if (puzzleInput[y][x] == '#' && visibleOccupiedSeats >= 5)
                        {
                            temp[y] += 'L';
                        }
                        else if (puzzleInput[y][x] == 'L' && visibleOccupiedSeats == 0)
                        {
                            temp[y] += '#';
                        }
                        else 
                        {
                            temp[y] += puzzleInput[y][x];
                        }                    
                    }
                }
                if (puzzleInput.SequenceEqual(temp))
                {
                    break;
                }
                puzzleInput = temp;
            }

            total = countOccupiedSeats(puzzleInput);
            Console.WriteLine($"Part 2 occupied seats: {total}");
        }

        static int countOccupiedSeats(string[] input)
        {
            int occupiedSeats = 0;
            for (int y = 0; y < input.Length; y++)
            {
                for (int x = 0; x < input[0].Length; x++)
                {
                    if (input[y][x] == '#')
                    {
                        occupiedSeats++;
                    }
                }
            }
            return occupiedSeats;
        }


        static int UpLeft(string[] input, int y, int x)
        {
            y -= 1;
            x -= 1;
            if (y < 0 || x < 0 || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return UpLeft(input, y, x);
        }
        static int Up(string[] input, int y, int x)
        {
            y -= 1;
            if (y < 0 || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return Up(input, y, x);
        }
        static int UpRight(string[] input, int y, int x)
        {
            y -= 1;
            x += 1;
            if (y < 0 || x == input[0].Length || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return UpRight(input, y, x);
        }
 
        static int Left(string[] input, int y, int x)
        {
            x -= 1;
            if (x < 0 || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return Left(input, y, x);
        }
        static int Right(string[] input, int y, int x)
        {
            x += 1;
            if (x == input[0].Length || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return Right(input, y, x);
        }

        static int DownLeft(string[] input, int y, int x)
        {
            y += 1;
            x -= 1;
            if (y == input.Length || x < 0 || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return DownLeft(input, y, x);
        }
        static int Down(string[] input, int y, int x)
        {
            y += 1;
            if (y == input.Length || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return Down(input, y, x);
        }
        static int DownRight(string[] input, int y, int x)
        {
            y += 1;
            x += 1;
            if (y == input.Length || x == input[0].Length || input[y][x] == 'L')
            {
                return 0;
            }
            else if (input[y][x] == '#')
            {
                return 1;
            }
            return DownRight(input, y, x);
        }
    }

}
