using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

var lines = File.ReadAllLines("input.txt");
var foods = GetFoods().ToList();
var allergens = foods.SelectMany(x => x.Allergens).Distinct().ToArray();
var ingredients = foods.SelectMany(x => x.Ingredients).Distinct().ToArray();

Console.WriteLine(PartOne());
Console.WriteLine(PartTwo());

int PartOne()
{
  var ingredientCandidates = new HashSet<string>();
  foreach (var allergen in allergens)
  {
    var food = foods.First(x => x.HasAllergen(allergen));
    var candidates = new HashSet<string>(food.Ingredients);
    foreach (var f in foods.Where(x => x.HasAllergen(allergen)))
      candidates.IntersectWith(f.Ingredients);
    ingredientCandidates.UnionWith(candidates);
  }
  var safeIngredients = new HashSet<string>(ingredients);
  safeIngredients.ExceptWith(ingredientCandidates);

  return safeIngredients.Select(ingredient => foods.Where(food => food.HasIngredient(ingredient)).Count()).Sum();
}

string PartTwo()
{
  var allergeneToIngredientsCandidates = new Dictionary<string, HashSet<string>>();
  foreach (var allergen in allergens)
  {
    var food = foods.First(x => x.HasAllergen(allergen));
    var candidates = new HashSet<string>(food.Ingredients);
    foreach (var f in foods.Where(x => x.HasAllergen(allergen)))
      candidates.IntersectWith(f.Ingredients);
    allergeneToIngredientsCandidates[allergen] = candidates;
  }

  var result = new Dictionary<string, string>();
  while (allergeneToIngredientsCandidates.Any())
  {
    var singles = allergeneToIngredientsCandidates
      .Where(x => x.Value.Count == 1)
      .Select(x => new { Allergen = x.Key, Ingredient = x.Value.Single() });
    foreach (var x in singles)
    {
      result[x.Allergen] = x.Ingredient;
      allergeneToIngredientsCandidates.Remove(x.Allergen);
      foreach (var kvp in allergeneToIngredientsCandidates)
        kvp.Value.Remove(x.Ingredient);
    }
  }
  return string.Join(",", result.OrderBy(x => x.Key).Select(x => x.Value));
}

IEnumerable<Food> GetFoods()
{
  foreach (var line in lines)
  {
    var index = line.IndexOf("(");
    var ingredients = line[0..(index - 1)].Split(" ");
    var allergens = line[(index + "(contains ".Length)..^1].Split(", ");
    yield return new Food
    {
      Ingredients = ingredients,
      Allergens = allergens
    };
  }
}

class Food
{
  public string[] Ingredients { get; set; }
  public string[] Allergens { get; set; }

  public bool HasIngredient(string ingredient) => Ingredients.Contains(ingredient);
  public bool HasAllergen(string allergen) => Allergens.Contains(allergen);
}
