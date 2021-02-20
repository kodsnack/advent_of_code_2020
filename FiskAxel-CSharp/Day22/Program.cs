using System;
using System.IO;
using System.Collections.Generic;

namespace Day22
{
    class Program
    {
        // Stökig kod
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput22.txt");
            
            // PARSE INPUT
            List<int> player1 = new List<int>();
            List<int> player2 = new List<int>();
            bool pl1 = false;       
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                if (puzzleInput[i] == "")
                {
                    continue;
                }
                else if (puzzleInput[i] == "Player 1:")
                {
                    pl1 = true;
                    continue;
                }
                else if (puzzleInput[i] == "Player 2:")
                {
                    pl1 = false;
                    continue;
                }

                if (pl1)
                {
                    int card = int.Parse(puzzleInput[i]);
                    player1.Add(card);
                }
                else
                {
                    int card = int.Parse(puzzleInput[i]);
                    player2.Add(card);
                } 
            }



            while (player1.Count != 0 && player2.Count != 0)
            {
                if (player1[0] > player2[0])
                {
                    player1.Add(player1[0]);
                    player1.RemoveAt(0);
                    player1.Add(player2[0]);
                    player2.RemoveAt(0);
                }
                else
                {
                    player2.Add(player2[0]);
                    player2.RemoveAt(0);
                    player2.Add(player1[0]);
                    player1.RemoveAt(0);
                }
            }

            int score = 0;
            for (int i = 0; i < player1.Count; i++)
            {
                score += player1[i] * (player1.Count - i);
            }
            for (int i = 0; i < player2.Count; i++)
            {
                score += player2[i] * (player2.Count - i);
            }

            Console.WriteLine("Part 1: ");
            Console.WriteLine(score);

            // PARSE INPUT
            player1.Clear();
            player2.Clear();
            pl1 = false;
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                if (puzzleInput[i] == "")
                {
                    continue;
                }
                else if (puzzleInput[i] == "Player 1:")
                {
                    pl1 = true;
                    continue;
                }
                else if (puzzleInput[i] == "Player 2:")
                {
                    pl1 = false;
                    continue;
                }

                if (pl1)
                {
                    int card = int.Parse(puzzleInput[i]);
                    player1.Add(card);
                }
                else
                {
                    int card = int.Parse(puzzleInput[i]);
                    player2.Add(card);
                }
            }

            List<string> p1old = new List<string>();
            List<string> p2old = new List<string>();

            while (player1.Count != 0 && player2.Count != 0)
            {
                string p1str = "";
                for (int i = 0; i < player1.Count; i++)
                {
                    p1str += player1[i];
                    p1str += ',';
                }
                string p2str = "";
                for (int i = 0; i < player2.Count; i++)
                {
                    p2str += player2[i];
                    p2str += ',';
                }
                if (p1old.Contains(p1str) || p2old.Contains(p2str))
                {
                    player2.Clear();
                    break;
                }
                p1old.Add(p1str);
                p2old.Add(p2str);

                if (player1[0] < player1.Count && player2[0] < player2.Count)
                {
                    bool player1win = false;
                    player1win = RecursiveCombat(player1, player1[0], player2, player2[0]);
                    if (player1win)
                    {
                        player1.Add(player1[0]);
                        player1.RemoveAt(0);
                        player1.Add(player2[0]);
                        player2.RemoveAt(0);
                    }
                    else
                    {
                        player2.Add(player2[0]);
                        player2.RemoveAt(0);
                        player2.Add(player1[0]);
                        player1.RemoveAt(0);
                    }
                }
                else if (player1[0] > player2[0])
                {
                    player1.Add(player1[0]);
                    player1.RemoveAt(0);
                    player1.Add(player2[0]);
                    player2.RemoveAt(0);
                }
                else
                {
                    player2.Add(player2[0]);
                    player2.RemoveAt(0);
                    player2.Add(player1[0]);
                    player1.RemoveAt(0);
                }
            }

            score = 0;
            for (int i = 0; i < player1.Count; i++)
            {
                score += player1[i] * (player1.Count - i);
            }
            for (int i = 0; i < player2.Count; i++)
            {
                score += player2[i] * (player2.Count - i);
            }
            Console.WriteLine("Part 2: ");
            Console.WriteLine(score);
        }

        static bool RecursiveCombat(List<int> player1, int len1, List<int> player2, int len2)
        {
            List<int> p1 = new List<int>();
            List<int> p2 = new List<int>();
            for (int i = 1; i <= len1; i++)
            {
                p1.Add(player1[i]);
            }
            for (int i = 1; i <= len2; i++)
            {
                p2.Add(player2[i]);
            }

            List<string> p1old = new List<string>();
            List<string> p2old = new List<string>();

            while (p1.Count != 0 && p2.Count != 0)
            {
                string p1str = "";
                for (int i = 0; i < p1.Count; i++)
                {
                    p1str += p1[i];
                    p1str += ',';
                }
                string p2str = "";
                for (int i = 0; i < p2.Count; i++)
                {
                    p2str += p2[i];
                    p2str += ',';
                }
                if (p1old.Contains(p1str) || p2old.Contains(p2str))
                {
                    p2.Clear();
                    break;
                }
                p1old.Add(p1str);
                p2old.Add(p2str);
                if (p1[0] < p1.Count && p2[0] < p2.Count)
                {
                    bool p1win = false;
                    p1win = RecursiveCombat(p1, p1[0], p2, p2[0]);
                    if (p1win)
                    {
                        p1.Add(p1[0]);
                        p1.RemoveAt(0);
                        p1.Add(p2[0]);
                        p2.RemoveAt(0);
                    }
                    else
                    {
                        p2.Add(p2[0]);
                        p2.RemoveAt(0);
                        p2.Add(p1[0]);
                        p1.RemoveAt(0);
                    }
                }
                else if (p1[0] > p2[0])
                {
                    p1.Add(p1[0]);
                    p1.RemoveAt(0);
                    p1.Add(p2[0]);
                    p2.RemoveAt(0);
                }
                else
                {
                    p2.Add(p2[0]);
                    p2.RemoveAt(0);
                    p2.Add(p1[0]);
                    p1.RemoveAt(0);
                }
            }

            if (p1.Count == 0)
            {
                return false;
            }
            else
            {
                return true;
            }
        }
    }
}