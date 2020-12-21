using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using AdventOfCode;

namespace day21
{
    public class Day21
    {
        readonly static string nsname = typeof(Day21).Namespace;
        readonly static string inputPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, @"..\..\..\..\" + nsname + "\\input.txt");

        // Day 21: Allergen Assessment - Map allergenes/ingredients to/from foods, logic & set theory

        static HashSet<string> ingredients = new HashSet<string>();
        static HashSet<string> allergenes = new HashSet<string>();

        static Dictionary<int, HashSet<string>> foodIngredients = new Dictionary<int, HashSet<string>>();
        static Dictionary<int, HashSet<string>> foodAllergenes = new Dictionary<int, HashSet<string>>();

        static Dictionary<string, HashSet<int>> foodsWithIngredient = new Dictionary<string, HashSet<int>>();
        static Dictionary<string, HashSet<int>> foodsWithAllergen = new Dictionary<string, HashSet<int>>();

        static void ReadInput(string path)
        {
            var input = ReadIndata.Strings(path);
            void AddFoodToItem(Dictionary<string, HashSet<int>> col, string key, int f)
            {
                if (!col.ContainsKey(key))
                    col[key] = new HashSet<int>();
                col[key].Add(f);
            }
            void AddItemToFood(Dictionary<int, HashSet<string>> col, int f, string item)
            {
                if (!col.ContainsKey(f))
                    col[f] = new HashSet<string>();
                col[f].Add(item);
            }
            int food = 0;
            foreach (string s in input)
            {
                var v = s.Split("(contains");
                var iv = v[0].Split(" ", StringSplitOptions.RemoveEmptyEntries);
                var av = v[1].Split(" ,)".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                {
                    foreach (string ingr in iv)
                    {
                        ingredients.Add(ingr);
                        AddItemToFood(foodIngredients, food, ingr);
                        AddFoodToItem(foodsWithIngredient, ingr, food);
                    }
                }
                {
                    foreach (string aller in av)
                    {
                        allergenes.Add(aller);
                        AddItemToFood(foodAllergenes, food, aller);
                        AddFoodToItem(foodsWithAllergen, aller, food);
                    }
                }
                food++;
            }
        }

        static Dictionary<string, HashSet<string>> allergeneCandidates = new Dictionary<string, HashSet<string>>();

        static Object PartA()
        {
            ReadInput(inputPath);

            // Allergene candidate: An ingredient that is in all foods where the allergene is
            var allCandidates = new HashSet<string>();
            foreach (var a in allergenes)
            {
                int food1 = foodsWithAllergen[a].First();
                var candidates = new HashSet<string>(foodIngredients[food1]);
                foreach (var food in foodsWithAllergen[a])
                    candidates.IntersectWith(foodIngredients[food]);
                allergeneCandidates[a] = candidates;
                allCandidates.UnionWith(candidates);
            }
            var safeIngredients = new HashSet<string>(ingredients);
            safeIngredients.ExceptWith(allCandidates);
            int ans = safeIngredients.Select(x => foodsWithIngredient[x].Count).Sum();
            Console.WriteLine("Part A: Result is {0}", ans);
            return ans;
        }

        static Object PartB()
        {
            var toGo = new Dictionary<string, HashSet<string>>(allergeneCandidates);
            var allergeneIngredient = new Dictionary<string, string>();
            while (toGo.Count > 0)
            {
                var found = toGo.Where(x => x.Value.Count == 1).ToDictionary(x => x.Key, x => x.Value.First());
                foreach (var (aller, ingr) in found)
                {
                    allergeneIngredient[aller] = ingr;
                    toGo.Remove(aller);
                    foreach (var (a, i) in toGo)
                        i.Remove(ingr);
                }
            }
            string ans = string.Join(",", allergeneIngredient.OrderBy(x => x.Key).Select(x => x.Value));
            Console.WriteLine("Part B: Result is {0}", ans);
            return ans;
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2020 - " + nsname + ":");
            PartA();
            PartB();
        }

        public static bool MainTest()
        {
            int a = 2324;
            string b = "bxjvzk,hqgqj,sp,spl,hsksz,qzzzf,fmpgn,tpnnkc";
            return (PartA().Equals(a)) && (PartB().Equals(b));
        }
    }
}
