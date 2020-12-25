using System;
using System.IO;

namespace Day17
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput17.txt");

            int xLength = puzzleInput[0].Length + 12;
            int yLength = puzzleInput.Length + 12;
            int zLength = 13;
            int wLength = zLength;
            
            int x = xLength / 2;
            int y = yLength / 2;
            int z = zLength / 2;
            int w = z;

            string[][] dimentions3 = new string[zLength][];
            string[][][] dimentions4 = new string[wLength][][];

            for (int i = 0; i < zLength; i++)
            {
                dimentions3[i] = new string[xLength];
                for (int j = 0; j < yLength; j++)
                {
                    for (int k = 0; k < xLength; k++)
                    {
                        dimentions3[i][j] += '.';
                    }
                }
            }
            for (int h = 0; h < wLength; h++)
            {  
                dimentions4[h] = new string[zLength][];
                for (int i = 0; i < zLength; i++)
                {
                    dimentions4[h][i] = new string[xLength];
                    for (int j = 0; j < yLength; j++)
                    {
                        for (int k = 0; k < xLength; k++)
                        {
                            dimentions4[h][i][j] += '.';
                        }
                    }
                }
            }

            int yStart = y - puzzleInput.Length / 2;
            int yEnd = yStart + puzzleInput.Length;
            int xStart = x - puzzleInput[0].Length / 2;
            int pIi = 0;
            for (int i = yStart; i < yEnd; i++)
            {
                dimentions3[z][i] = "";
                for (int j = 0; j < xStart; j++)
                {
                    dimentions3[z][i] += '.';
                }
                for (int j = 0; j < puzzleInput[0].Length; j++)
                {
                    dimentions3[z][i] += puzzleInput[pIi][j];
                }
                for (int j = 0; j < xStart; j++)
                {
                    dimentions3[z][i] += '.';
                }
                pIi++;
            }

            pIi = 0;
            for (int i = yStart; i < yEnd; i++)
            {
                dimentions4[w][z][i] = "";
                for (int j = 0; j < xStart; j++)
                {
                    dimentions4[w][z][i] += '.';
                }
                for (int j = 0; j < puzzleInput[0].Length; j++)
                {
                    dimentions4[w][z][i] += puzzleInput[pIi][j];
                }
                for (int j = 0; j < xStart; j++)
                {
                    dimentions4[w][z][i] += '.';
                }
                pIi++;
            }


            ////
            //// PART 1
            ////

            for (int a = 0; a < 6; a++)
            {
                string[][] temp = new string[zLength][];
                for (int i = 0; i < zLength; i++)
                {
                    temp[i] = new string[xLength];
                }

                for (int i = 0; i < zLength; i++)
                {
                    for (int j = 0; j < yLength; j++)
                    {
                        for (int k = 0; k < xLength; k++)
                        {
                            int cubes = countCubeNeighbours3D(i, j, k, dimentions3);
                            if (dimentions3[i][j][k] == '#' &&
                                cubes != 2 && cubes != 3)
                            {
                                temp[i][j] += '.';
                            }
                            else if (dimentions3[i][j][k] == '.' &&
                                     cubes == 3)
                            {
                                temp[i][j] += '#';
                            }
                            else
                            {
                                temp[i][j] += dimentions3[i][j][k];
                            }
                        }
                    }
                }
                dimentions3 = temp;
            }

            int finalCubesNum = 0;
            for (int i = 0; i < zLength; i++)
            {
                for (int j = 0; j < yLength; j++)
                {
                    for (int k = 0; k < xLength; k++)
                    {
                        if (dimentions3[i][j][k] == '#')
                        {
                            finalCubesNum++;
                        }
                    }
                }
            }

            Console.Write("Part 1: ");
            Console.WriteLine(finalCubesNum);


            ////
            //// PART 2
            ////

            for (int a = 0; a < 6; a++)
            {
                string[][][] temp = new string[wLength][][];
                for (int i = 0; i < wLength; i++)
                {
                    temp[i] = new string[zLength][];
                    for (int j = 0; j < zLength; j++)
                    {
                        temp[i][j] = new string[yLength];
                    }
                }

                for (int h = 0; h < wLength; h++)
                {
                    for (int i = 0; i < zLength; i++)
                    {
                        for (int j = 0; j < yLength; j++)
                        {
                            for (int k = 0; k < xLength; k++)
                            {
                                int cubes = countCubeNeighbours4D(h, i, j, k, dimentions4);
                                if (dimentions4[h][i][j][k] == '#' &&
                                    cubes != 2 && cubes != 3)
                                {
                                    temp[h][i][j] += '.';
                                }
                                else if (dimentions4[h][i][j][k] == '.' &&
                                         cubes == 3)
                                {
                                    temp[h][i][j] += '#';
                                }
                                else
                                {
                                    temp[h][i][j] += dimentions4[h][i][j][k];
                                }
                            }
                        }
                    }
                }
                dimentions4 = temp;
            }

            finalCubesNum = 0;
            for (int h = 0; h < wLength; h++)
            { 
                for (int i = 0; i < zLength; i++)
                {
                    for (int j = 0; j < yLength; j++)
                    {
                        for (int k = 0; k < xLength; k++)
                        {
                            if (dimentions4[h][i][j][k] == '#')
                            {
                                finalCubesNum++;
                            }
                        }
                    }
                }
            }

            Console.Write("Part 2: ");
            Console.WriteLine(finalCubesNum);
        }

        static int countCubeNeighbours3D(int z, int y, int x, string[][] arr)
        {
            int result = 0;
            for (int i = 0; i < 3; i++)
            {
                for (int j = 0; j < 3; j++)
                {
                    for (int k = 0; k < 3; k++)
                    {
                        if (i == 1 && j == 1 && k == 1)
                        {
                            //nothing
                        }
                        else if((z - 1 + i) < arr.Length && (z - 1 + i) > 0 &&
                                (y - 1 + j) < arr[0].Length && (y - 1 + j) > 0 &&
                                (x - 1 + k) < arr[0][0].Length && (x - 1 + k) > 0 &&
                                arr[z - 1 + i][y - 1 + j][x - 1 + k] == '#')
                        {
                            result++;
                        }
                    }
                }
            }

            return result;
        }

        static int countCubeNeighbours4D(int w, int z, int y, int x, string[][][] arr)
        {
            int result = 0;
            for (int h = 0; h < 3; h++)
            {
                for (int i = 0; i < 3; i++)
                {
                    for (int j = 0; j < 3; j++)
                    {
                        for (int k = 0; k < 3; k++)
                        {
                            if (h == 1 && i == 1 && j == 1 && k == 1)
                            {
                                //nothing
                            }
                            else if ((w - 1 + h) < arr.Length && (w - 1 + h) > 0 &&
                                     (z - 1 + i) < arr[0].Length && (z - 1 + i) > 0 &&
                                     (y - 1 + j) < arr[0][0].Length && (y - 1 + j) > 0 &&
                                     (x - 1 + k) < arr[0][0][0].Length && (x - 1 + k) > 0 &&
                                     arr[w - 1 + h][z - 1 + i][y - 1 + j][x - 1 + k] == '#')
                            {
                                result++;
                            }
                        }
                    }
                }
            }

            return result;
        }
    }
}