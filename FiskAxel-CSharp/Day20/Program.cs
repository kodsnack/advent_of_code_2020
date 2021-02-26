using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Day20
{
    struct Connection
    {
        public string From;
        public string To;
        public Tile Tile;

        public Connection(string from, string to, Tile tile)
        {
            this.From = from;
            this.To = to;
            this.Tile = tile;
        }
    }

    class Tile
    {
        public string Id;
        public string[] Data;
        public string[] Borders;
        public string Up;
        public string Right;
        public string Down;
        public string Left;
        public List<Connection> Connections;
        public string State;

        public Tile(string id)
        {
            this.Id = id;
            this.Data = new string[8];
            this.Borders = new string[4];
            this.Up = null;
            this.Right = null;
            this.Down = null;
            this.Left = null;
            this.Connections = new List<Connection>();
            this.State = "0";
        }
        // Right
        public Tile Turn()
        {
            string up = this.Up;
            string right = this.Right;
            string down = this.Down;
            string left = this.Left;
            string bUp = this.Borders[0];
            string bRight = this.Borders[1];
            string bDown = this.Borders[2];
            string bLeft = this.Borders[3];
            int stateNum = int.Parse(this.State.Substring(0, 1));
            if (stateNum == 0 || stateNum == 2)
            {
                this.Up = left;
                this.Right = up;
                this.Down = right;
                this.Left = down;
 
            }
            else
            {
                this.Up = ReverseTileSide(left);
                this.Right = ReverseTileSide(up);
                this.Down = ReverseTileSide(right);
                this.Left = ReverseTileSide(down);
            }               
            this.Borders[0] = Reverse(bLeft);
            this.Borders[1] = bUp;
            this.Borders[2] = Reverse(bRight);
            this.Borders[3] = bDown;
            this.Data = UpdateDataTurn(this.Data);
            this.State = UpdateState(this.Up, this.Right);
            return this;
        }
        // Vertical
        public Tile Flip()
        {
            string up = this.Up;
            string right = this.Right;
            string down = this.Down;
            string left = this.Left;
            string bUp = this.Borders[0];
            string bRight = this.Borders[1];
            string bDown = this.Borders[2];
            string bLeft = this.Borders[3];
            this.Up = ReverseTileSide(up);
            this.Right = left;
            this.Down = ReverseTileSide(down);
            this.Left = right;
            this.Data = UpdateDataFlip(this.Data);
            this.State = UpdateState(this.Up, this.Right);
            this.Borders[0] = Reverse(bUp);
            this.Borders[1] = bLeft;
            this.Borders[2] = Reverse(bDown);
            this.Borders[3] = bRight;
            return this;
        }

        private string ReverseTileSide(string input)
        {
            if (input.Length == 1)
            {
                return input += "R";
            }
            else
            {
                return input.Substring(0, 1);
            }
        }
        private string Reverse(string input)
        {
            string output = "";
            for (int i = input.Length - 1; i >= 0; i--)
            {
                output += input[i];
            }
            return output;
        }
        private string UpdateState(string u, string r)
        {
            if (u == "0" && r == "1")
            {
                return "0";
            }
            if (u == "3" && r == "0")
            {
                return "1";
            }
            if (u == "2R" && r == "3R")
            {
                return "2";
            }
            if (u == "1R" && r == "2R")
            {
                return "3";
            }

            if (u == "0R" && r == "3")
            {
                return "0V";
            }
            if (u == "1" && r == "0R")
            {
                return "1V";
            }
            if (u == "2" && r == "1R")
            {
                return "2V";
            }
            if (u == "3R" && r == "2")
            {
                return "3V";
            }
            return "";
        }
        private string[] UpdateDataTurn(string[] data)
        {
            string[] upDatad = new string[data.Length];
            for (int x = 0; x < data.Length; x++)
            {
                for (int y = data.Length - 1; y >= 0; y--)
                {
                    upDatad[x] += data[y][x];
                }
            }
            return upDatad;
        }
        private string[] UpdateDataFlip(string[] data)
        {
            string[] upDatad = new string[data.Length];
            for (int y = 0; y < data.Length; y++)
            {
                for (int x = data.Length - 1; x >= 0; x--)
                {
                    upDatad[y] += data[y][x];
                }
            }
            return upDatad;
        }
        
    }

    class Program
    {
        static void Main(string[] args)
        {
            /// INPUT PARSING   
                
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput20.txt");

            Dictionary<string, Tile> tiles = new Dictionary<string, Tile>();
            List<Tile> puzzlePieces = new List<Tile>();
            for (int i = 0; i < puzzleInput.Length; i += 12)
            {
                string id = puzzleInput[i].Substring(5, 4);
                Tile newTile = new Tile(id);
                newTile.Borders[0] = puzzleInput[i + 1];
                newTile.Borders[2] = puzzleInput[i + 10];
                for (int j = 1; j < 11; j++)
                {
                    newTile.Borders[1] += puzzleInput[i + j][9];
                    newTile.Borders[3] += puzzleInput[i + j][0];
                }
                for (int j = 0; j < 8; j++)
                {
                    newTile.Data[j] += puzzleInput[i + 2 + j].Substring(1, 8);
                }
                newTile.Up = "0";
                newTile.Right = "1";
                newTile.Down = "2";
                newTile.Left = "3";
                puzzlePieces.Add(newTile);
                tiles.Add(newTile.Id, newTile);
            }
                
            ////
            //// PART 1 
            ////

            List<long> corners = new List<long>();
            for (int i = 0; i < puzzlePieces.Count; i++)
            {
                for (int j = 0; j < 4; j++)
                {
                    for (int k = 0; k < puzzlePieces.Count; k++)
                    {
                        if (i == k) { continue; }
                        for (int l = 0; l < 4; l++)
                        {
                            if (puzzlePieces[i].Borders[j] == (puzzlePieces[k].Borders[l]))
                            {
                                Connection newConnection = new Connection(j.ToString(), l.ToString(), puzzlePieces[k]);
                                puzzlePieces[i].Connections.Add(newConnection);
                            }
                            else if (puzzlePieces[i].Borders[j] == Reverse(puzzlePieces[k].Borders[l]))
                            {
                                Connection newConnection = new Connection(j.ToString(), l + "R", puzzlePieces[k]);
                                puzzlePieces[i].Connections.Add(newConnection);
                            }
                        }
                    }
                }
                if (puzzlePieces[i].Connections.Count == 2)
                {
                    corners.Add(long.Parse(puzzlePieces[i].Id));
                }                   
            }

            long result = corners[0] * corners[1] * corners[2] * corners[3];
            Console.WriteLine("Part 1: ");
            Console.WriteLine(result);

            ////
            //// PART 2
            ////

            int width = Convert.ToInt32(Math.Sqrt(Convert.ToDouble(tiles.Count)));
            string[,] puzzleMap = new string[width, width];
            List<string> usedTiles = new List<string>();

            puzzleMap[0, 0] = corners[0].ToString();
            puzzleMap[0, 1] = tiles[puzzleMap[0, 0]].Connections[1].Tile.Id;
            usedTiles.Add(puzzleMap[0, 0]);
            usedTiles.Add(puzzleMap[0, 1]);
            string nextNum = NextSideNum(tiles[puzzleMap[0, 0]].Connections[1].To.Substring(0, 1));

            ///
            /// PUZZLE ORDER
            ///
            // Top rows           
            for (int x = 2; x < width; x++)
            {
                for (int j = 0; j < 3; j++)
                {
                    if (tiles[puzzleMap[0, x - 1]].Connections[j].From == nextNum)
                    {
                        puzzleMap[0, x] = tiles[puzzleMap[0, x - 1]].Connections[j].Tile.Id;
                        usedTiles.Add(puzzleMap[0, x]);
                        nextNum = NextSideNum(tiles[puzzleMap[0, x - 1]].Connections[j].To.Substring(0, 1));
                        break;
                    }
                }
            }
            // The rest of the rows
            for (int y = 1; y < width; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    for (int i = 0; i < 4; i++)
                    {
                        if (!usedTiles.Contains(tiles[puzzleMap[y - 1, x]].Connections[i].Tile.Id))
                        {
                            puzzleMap[y, x] = tiles[puzzleMap[y - 1, x]].Connections[i].Tile.Id;
                            usedTiles.Add(puzzleMap[y, x]);
                            break;
                        }
                    }
                }
            }

            ///
            /// TURN & FLIP TILES
            ///
            Tile current = tiles[puzzleMap[0, 0]];
            Tile rightTile = tiles[puzzleMap[0, 1]];
            Tile downTile = tiles[puzzleMap[1, 0]];
            string right = "";
            string down = "";
            // Top left corner
            for (int i = 0; i < 2; i++)
            { 
                if (current.Connections[i].Tile.Id == rightTile.Id)
                {
                    right = current.Connections[i].From;
                }
                else if (current.Connections[i].Tile.Id == downTile.Id)
                {
                    down = current.Connections[i].From;
                }
            }
            int counter = 1;
            while ((current.Right != right && current.Right != right + "R")
                || (current.Down != down && current.Down != down + "R"))
            {
                FlipAndTurn(current, counter, out counter);
            }
            // The rest of the tiles
            for (int y = 0; y < width; y++)
            {
                if (y != 0)
                {
                    current = tiles[puzzleMap[y, 0]];
                    Tile upTile = tiles[puzzleMap[y - 1, 0]];
                    counter = 1;
                    while (current.Borders[0] != upTile.Borders[2])
                    {
                        FlipAndTurn(current, counter, out counter);
                    }
                }
                for (int x = 1; x < width; x++)
                {
                    current = tiles[puzzleMap[y, x]];
                    Tile leftTile = tiles[puzzleMap[y, x - 1]];
                    counter = 1;
                    while (current.Borders[3] != leftTile.Borders[1])
                    {
                        FlipAndTurn(current, counter, out counter);
                    }
                }
            }

            string[] completePuzzle = new string[width * 8];
            for (int y = 0; y < width; y++)
            {   
                for (int j = 0; j < 8; j++)
                {
                    for (int x = 0; x < width; x++)
                    {
                        completePuzzle[j + 8 * y] += tiles[puzzleMap[y, x]].Data[j];
                    }
                } 
            }
            for (int i = 0; i < completePuzzle.Length; i++)
            {
                Console.WriteLine(completePuzzle[i]);
            }

            string[] seaMonster = new string[3];
            seaMonster[0] = "                  # ";
            seaMonster[1] = "#    ##    ##    ###";
            seaMonster[2] = " #  #  #  #  #  #   ";

            int seaMonsters = 0;
            
            for (int a = 0; a < 8; a++)
            {
                for (int y = 0; y < completePuzzle.Length - 3; y++)
                {
                    for (int x = 0; x < completePuzzle.Length - seaMonster[0].Length; x++)
                    {
                        bool monster = true;
                        for (int i = 0; i < 3; i++)
                        {
                            for (int j = 0; j < seaMonster[0].Length; j++)
                            {
                                if (seaMonster[i][j] == '#' && completePuzzle[y + i][x + j] != '#')
                                {
                                    monster = false;
                                    break;
                                }
                            }
                            if (!monster)
                            {
                                break;
                            }
                        }
                        if (monster)
                        {
                            seaMonsters++;
                        }
                    }
                }
                completePuzzle = TurnAndFlip(completePuzzle, a);
            }

            int totalHashes = 0;
            for (int y = 0; y < completePuzzle.Length; y++)
            {
                for (int x = 0; x < completePuzzle[0].Length; x++)
                {
                    if (completePuzzle[y][x] == '#')
                    {
                        totalHashes++;
                    }
                }
            }

            int waterRoughness = totalHashes - 15 * seaMonsters;
            Console.Write("Part 2: \nSea monsters: ");
            Console.WriteLine(seaMonsters);
            Console.Write("Water roughness: ");
            Console.WriteLine(waterRoughness);
        }


        static void FlipAndTurn(Tile tile, int incounter ,out int counter)
        {
            counter = incounter;
            if (counter == 4)
            {
                tile = tile.Flip();
                counter = 1;
                return;
            }
            tile = tile.Turn();
            counter++;
        }
        static string[] TurnAndFlip(string[] input, int counter)
        {
            if (counter == 3)
            {
                return input = Flip(input);
            }
            return input = Turn(input);
        }
        static string[] Turn(string[] data)
        {
            string[] upDatad = new string[data.Length];
            for (int x = 0; x < data.Length; x++)
            {
                for (int y = data.Length - 1; y >= 0; y--)
                {
                    upDatad[x] += data[y][x];
                }
            }
            return upDatad;
        }
        static string[] Flip(string[] data)
        {
            string[] upDatad = new string[data.Length];
            for (int y = 0; y < data.Length; y++)
            {
                for (int x = data.Length - 1; x >= 0; x--)
                {
                    upDatad[y] += data[y][x];
                }
            }
            return upDatad;
        }
        static string NextSideNum(string input)
        {
            if (input == "0")
            {
                return "2";
            }
            else if (input == "1")
            {
                return "3";
            }
            else if (input == "2")
            {
                return "0";
            }
            else if (input == "3")
            {
                return "1";
            }

            return "0";
        }
        static string Reverse(string input)
        {
            string output = "";
            for (int i = input.Length - 1; i >= 0; i--)
            {
                output += input[i];
            }
            return output;
        }
    }
}