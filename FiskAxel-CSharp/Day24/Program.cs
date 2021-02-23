using System;
using System.IO;
using System.Collections.Generic;

namespace Day24
{
    struct Coordinate
    {
        public int x;
        public int y;
        public Coordinate(int x, int y)
        {
            this.x = x;
            this.y = y;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput24.txt");

            List<Coordinate> blackTiles = new List<Coordinate>();
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                Coordinate tile = new Coordinate(0, 0);
                for (int j = 0; j < puzzleInput[i].Length; j++)
                {
                    if (puzzleInput[i][j] == 'n')
                    {
                        if (puzzleInput[i][j + 1] == 'w')
                        {
                            tile.x--;
                            tile.y++;
                        }
                        else if (puzzleInput[i][j + 1] == 'e')
                        {
                            tile.x++;
                            tile.y++;
                        }
                        j++;
                    }
                    else if (puzzleInput[i][j] == 's')
                    {
                        if (puzzleInput[i][j + 1] == 'w')
                        {
                            tile.x--;
                            tile.y--;
                        }
                        else if (puzzleInput[i][j + 1] == 'e')
                        {
                            tile.x++;
                            tile.y--;
                        }
                        j++;
                    }
                    else if (puzzleInput[i][j] == 'w')
                    {
                        tile.x -= 2;
                    }
                    else if (puzzleInput[i][j] == 'e')
                    {
                        tile.x += 2;
                    }
                }

                if (blackTiles.Contains(tile))
                {
                    blackTiles.Remove(tile);
                }
                else
                {
                    blackTiles.Add(tile);
                }
            }

            Console.WriteLine("Part 1: ");
            Console.WriteLine(blackTiles.Count);

            List<Coordinate> whiteTiles = new List<Coordinate>();
            for (int i = 0; i < 100; i++)
            {
                for (int j = 0; j < blackTiles.Count; j++)
                {
                    Coordinate temp = blackTiles[j];
                    temp.x -= 1;
                    temp.y += 1;
                    if (!blackTiles.Contains(temp) && !whiteTiles.Contains(temp))
                    {
                        whiteTiles.Add(temp);
                    }
                    temp = blackTiles[j];
                    temp.x += 1;
                    temp.y += 1;
                    if(!blackTiles.Contains(temp) && !whiteTiles.Contains(temp))
                    {
                        whiteTiles.Add(temp);
                    }
                    temp = blackTiles[j];
                    temp.x -= 1;
                    temp.y -= 1;
                    if (!blackTiles.Contains(temp) && !whiteTiles.Contains(temp))
                    {
                        whiteTiles.Add(temp);
                    }
                    temp = blackTiles[j];
                    temp.x += 1;
                    temp.y -= 1;
                    if (!blackTiles.Contains(temp) && !whiteTiles.Contains(temp))
                    {
                        whiteTiles.Add(temp);
                    }
                    temp = blackTiles[j];
                    temp.x += 2;
                    if (!blackTiles.Contains(temp) && !whiteTiles.Contains(temp))
                    {
                        whiteTiles.Add(temp);
                    }
                    temp = blackTiles[j];
                    temp.x -= 2;
                    if (!blackTiles.Contains(temp) && !whiteTiles.Contains(temp))
                    {
                        whiteTiles.Add(temp);
                    }
                }

                List<int> whiteFlip = new List<int>();
                for (int j = 0; j < whiteTiles.Count; j++)
                {
                    int adjacentTiles = CountAdjacentBlackTiles(blackTiles, whiteTiles[j]);
                    if (adjacentTiles == 2)
                    {
                        whiteFlip.Add(j);
                    }
                }

                List<int> blackFlip = new List<int>();
                for (int j = 0; j < blackTiles.Count; j++)
                {
                    int adjacentTiles = CountAdjacentBlackTiles(blackTiles, blackTiles[j]);
                    if (adjacentTiles != 1 && adjacentTiles != 2)
                    {
                        blackFlip.Add(j);
                    }
                }

                List<Coordinate> newBlack = new List<Coordinate>();
                List<Coordinate> newWhite = new List<Coordinate>();
                for (int j = whiteFlip.Count; j > 0; j--)
                {
                    newBlack.Add(whiteTiles[whiteFlip[j - 1]]);
                    whiteTiles.RemoveAt(whiteFlip[j - 1]);
                }
                for (int j = blackFlip.Count; j > 0; j--)
                {
                    newWhite.Add(blackTiles[blackFlip[j - 1]]);
                    blackTiles.RemoveAt(blackFlip[j - 1]);
                }
                foreach (Coordinate tile in newBlack)
                {
                    blackTiles.Add(tile);
                }
                foreach (Coordinate tile in newWhite)
                {
                    whiteTiles.Add(tile);
                }

                Console.WriteLine($"Day {i + 1}: {blackTiles.Count}");
            }

            Console.WriteLine("Part 2:");
            Console.WriteLine(blackTiles.Count);
            // 2:54 for part 2.
        }

        static int CountAdjacentBlackTiles(List<Coordinate> blacks, Coordinate tile)
        {
            int adjacentTiles = 0;
            Coordinate temp = tile;
            temp.x -= 1;
            temp.y += 1;
            if (blacks.Contains(temp))
            {
                adjacentTiles++;
            }
            temp = tile;
            temp.x += 1;
            temp.y += 1;
            if (blacks.Contains(temp))
            {
                adjacentTiles++;
            }
            temp = tile;
            temp.x -= 1;
            temp.y -= 1;
            if (blacks.Contains(temp))
            {
                adjacentTiles++;
            }
            temp = tile;
            temp.x += 1;
            temp.y -= 1;
            if (blacks.Contains(temp))
            {
                adjacentTiles++;
            }
            temp = tile;
            temp.x += 2;
            if (blacks.Contains(temp))
            {
                adjacentTiles++;
            }
            temp = tile;
            temp.x -= 2;
            if (blacks.Contains(temp))
            {
                adjacentTiles++;
            }

            return adjacentTiles;
        }
    }
}