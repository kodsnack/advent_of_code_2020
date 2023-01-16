import '../util/util.dart';

//const String inputFile = 'day21/example.txt';
const String inputFile = 'day21/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);

  // print('Part 2:');
  // final resultP2 = calcResultP2(inputLines);
  // print(resultP2);
}

int calcResultP1(List<String> inputLines) {
  Map<String, Set<String>> ingrWithAllerg = {};
  List<String> allIngredients = [];
  for (final line in inputLines) {
    Set<String> ingredients = line.split(' (contains')[0].split(' ').toSet();
    allIngredients.addAll(ingredients);
    Set<String> allergens =
        line.split(' (contains ')[1].replaceAll(')', '').split(', ').toSet();
    for (final allergen in allergens) {
      if (ingrWithAllerg.containsKey(allergen)) {
        // We already have this allergen. Remove all ingredients that are not in this line
        ingrWithAllerg[allergen] =
            ingrWithAllerg[allergen]!.intersection(ingredients);
      } else {
        ingrWithAllerg[allergen] = ingredients;
      }
    }
  }

  Map<String, String> allergWithIngr = {};
  bool ready = false;
  while (!ready) {
    ready = true;
    for (final ingredient in ingrWithAllerg.keys) {
      final allergens = ingrWithAllerg[ingredient]!;
      if (allergens.length == 1) {
        // Ingredient has only one allergen so we can add it to the allergen set
        allergWithIngr[allergens.single] = ingredient;
      } else {
        ready = false;
        Set<String> newAllergenSet = {};

        // If we have an allergen that is already found we can remove it from this set
        for (final allergen in allergens) {
          if (!allergWithIngr.containsKey(allergen))
            newAllergenSet.add(allergen);
        }
        ingrWithAllerg[ingredient] = newAllergenSet;
      }
    }
  }

  allIngredients.removeWhere((element) => allergWithIngr.containsKey(element));

  print('Part 2 : ');
  final dangerList = allergWithIngr.keys.toList();
  dangerList.sort(((a, b) => allergWithIngr[a]!.compareTo(allergWithIngr[b]!)));
  print(dangerList.join(','));
  return allIngredients.length;
}
