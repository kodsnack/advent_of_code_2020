using System;
using System.Collections.Generic;
using System.IO;

namespace Day07
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput7.txt");

            ////
            //// PART 1
            ////

            List<string> bagsContainingSGB = new List<string>();
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                string[] words = puzzleInput[i].Split("s contain");
                if (words[1].Contains("shiny gold bag"))
                {
                    bagsContainingSGB.Add(words[0]);
                }
            }
            
            int newBags = 1; 
            while (newBags > 0)
            {
                List<string> temp = new List<String>();
                newBags = 0;
                for (int i = 0; i < puzzleInput.Length; i++)
                {
                    string[] words = puzzleInput[i].Split("s contain");
                    for (int j = 0; j < bagsContainingSGB.Count; j++)
                    {
                        if (words[1].Contains(bagsContainingSGB[j]))
                        {
                            temp.Add(words[0]);
                            
                            break;
                        }
                    }
                }
                foreach (string newBag in temp)
                {
                    bool add = true;
                    foreach (string bag in bagsContainingSGB)
                    {
                        if (String.Equals(bag, newBag))
                        {
                            add = false;
                            break;
                        }
                    }
                    if (add)
                    {
                        newBags++;
                        bagsContainingSGB.Add(newBag);
                    }    
                }
            }

            Console.Write("Part 1: ");
            Console.WriteLine(bagsContainingSGB.Count);


            ////
            //// PART 2
            ////

            List<string> bagsInSGB = new List<string>();
            int numBagsInSBS = 0;
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                string[] words = puzzleInput[i].Split("s contain ");
                if (words[0].Contains("shiny gold bag"))
                {
                    string[] bags = words[1].Split(", ");
                    foreach (string bag in bags)
                    {
                        string num = ""; 
                        num += bag[0];
                        int amount = int.Parse(num);
                        string parsedBag = ParseBag(bag);
                        for (int j = 0; j < amount; j++)
                        {
                            bagsInSGB.Add(parsedBag);
                            numBagsInSBS++;
                        }
                    }
                    
                }
            }

            newBags = 1;
            while (newBags > 0)
            {
                newBags = 0;
                List<string> temp = new List<String>();           
                for (int i = 0; i < puzzleInput.Length; i++)
                {
                    string[] words = puzzleInput[i].Split("s contain ");
                    for (int j = 0; j < bagsInSGB.Count; j++)
                    {
                        if (words[0].Contains(bagsInSGB[j]))
                        {
                            string[] bags = words[1].Split(", ");
                            foreach (string bag in bags)
                            {
                                if (bag[0] == 'n')
                                { break; }
                                string num = "";
                                num += bag[0];
                                
                                int amount = int.Parse(num);
                                string parsedBag = ParseBag(bag);
                                for (int k = 0; k < amount; k++)
                                {
                                    temp.Add(parsedBag);
                                    numBagsInSBS++;
                                    newBags++;
                                }
                            }
                        }
                    }  
                }
                bagsInSGB.Clear();
                foreach (string bag in temp)
                {
                    bagsInSGB.Add(bag);
                }
            }
            Console.Write("Part 2: ");
            Console.WriteLine(numBagsInSBS);
        }
        
        static string ParseBag(string bag)
        {
            bag = bag.Substring(1);
            bag = bag.Substring(1);
            if (bag[bag.Length - 1] == '.')
            {
                bag = bag.Remove(bag.Length - 1);
            }
            if (bag[bag.Length - 1] == 's')
            {
                bag = bag.Remove(bag.Length - 1);
            }
            return bag;
        }
    }  
}