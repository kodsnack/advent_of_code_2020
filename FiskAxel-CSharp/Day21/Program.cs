using System;
using System.Collections.Generic;
using System.IO;

namespace Day21
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] puzzleInput = File.ReadAllLines("../../../puzzleInput21.txt");

            List<string> allergenes = new List<string>();
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                int start = puzzleInput[i].IndexOf("(contains ") + 10;
                string contains = puzzleInput[i].Substring(start);
                contains = contains.Remove(contains.Length -1);
                string[] split = contains.Split(", ");
                foreach (string item in split)
                {
                    if (!allergenes.Contains(item))
                    {
                        allergenes.Add(item);
                    }
                }
            }
            allergenes.Sort();
            string[] allergenIngredients = new string[allergenes.Count];

            List<string> possibleAllergenes = new List<string>();
            for (int i = 0; i < allergenes.Count; i++)
            {
                string possibles = "";
                bool first = true;
                for (int j = 0; j < puzzleInput.Length; j++)
                {
                    if (puzzleInput[j].Contains(allergenes[i]))
                    {
                        int end = puzzleInput[j].IndexOf(" (contains ");
                        string ingridients = puzzleInput[j].Substring(0, end);
                        string[] ingridient = ingridients.Split(" ");
                        if (first)
                        {
                            possibles = ingridients;
                            first = false;
                        }
                        else
                        {
                            string temp = "";
                            string[] splitted = possibles.Split(" ");
                            for (int k = 0; k < ingridient.Length; k++)
                            {
                                for (int l = 0; l < splitted.Length; l++)
                                {
                                    if (ingridient[k] == splitted[l])
                                    {
                                        temp += ingridient[k];
                                        temp += " ";
                                        break;
                                    }
                                }
                            }
                            possibles = temp.Trim();
                            allergenIngredients[i] = temp.Trim();
                        }
                    }   
                }
                string[] possible = possibles.Split(" ");
                foreach (string item in possible)
                {
                    if (!possibleAllergenes.Contains(item))
                    {
                        possibleAllergenes.Add(item);
                    }
                } 
            }

            int result = 0;
            for (int i = 0; i < puzzleInput.Length; i++)
            {
                int end = puzzleInput[i].IndexOf(" (contains ");
                string ingridients = puzzleInput[i].Substring(0, end);
                string[] ingridient = ingridients.Split(" ");
                for (int j = 0; j < ingridient.Length; j++)
                {
                    bool istrue = true;
                    for (int k = 0; k < possibleAllergenes.Count; k++)
                    {
                        if (ingridient[j] == possibleAllergenes[k])
                        {
                            istrue = false;
                            break;
                        }
                    }
                    if (istrue)
                    {
                        result++;
                    }
                }
            }

            Console.Write("Part 1: ");
            Console.WriteLine(result);

            List<string> removeIng = new List<string>();
            bool done = false;
            while (!done)
            {
                done = true;
                for (int i = 0; i < allergenIngredients.Length; i++)
                {
                    if (!allergenIngredients[i].Contains(' ') && !allergenIngredients[i].Contains(','))
                    {
                        removeIng.Add(allergenIngredients[i]);
                        allergenIngredients[i] += ',';
                        done = false;
                    }
                }
                for (int i = 0; i < allergenIngredients.Length; i++)
                {
                    for (int j = 0; j < removeIng.Count; j++)
                    {
                        if (allergenIngredients[i].Contains(removeIng[j]) && !allergenIngredients[i].Contains(','))
                        {
                            int start = allergenIngredients[i].IndexOf(removeIng[j]);
                            int leng = removeIng[j].Length;
                            allergenIngredients[i] = allergenIngredients[i].Remove(start, leng).Trim();
                        }
                    }
                }
            }

            string allerg = "";
            for (int i = 0; i < allergenIngredients.Length; i++)
            {
                allerg += allergenIngredients[i];
            }
            allerg = allerg.Remove(allerg.Length - 1);

            Console.Write("Part 2: ");
            Console.WriteLine(allerg);
        }
    }
}

