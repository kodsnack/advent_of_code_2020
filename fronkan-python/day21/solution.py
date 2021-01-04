from collections import defaultdict
from pathlib import Path
from typing import DefaultDict, Dict, List, Set, Counter as CounterType
from collections import Counter


class Product:
    all_ingredient_counts: CounterType[str] = Counter()
    all_allergens_counts: CounterType[str] = Counter()

    def __init__(self, ingredients: Set[str], allergens: Set[str]) -> None:
        self.ingredients = ingredients
        self.all_ingredient_counts.update(ingredients)
        self.allergens = allergens
        self.all_allergens_counts.update(allergens)

    @classmethod
    def reset_cls_counters(cls):
        cls.all_ingredient_counts = Counter()
        cls.all_allergens_counts = Counter()

    @classmethod
    def from_line(cls, line: str) -> "Product":
        ingredient_str, allergen_str = line.split("(")
        allergens = {
            a_cleaned
            for a in allergen_str.replace(")", "").replace("contains", "").split(",")
            if (a_cleaned := a.strip())
        }
        ingredients = {
            i_cleaned.strip()
            for i in ingredient_str.split(" ")
            if (i_cleaned := i.strip())
        }
        return cls(ingredients, allergens)

    def __repr__(self) -> str:
        return f"Product(ingredients={self.ingredients}, allergens={self.allergens})"

    @classmethod
    def products_from_file(cls, input_file: Path) -> List["Product"]:
        with open(input_file) as f:
            return [cls.from_line(l) for l in f]


def get_allergen_to_ingredient_mapping(products: List[Product]) -> Dict[str, str]:
    allergen_to_ingredient_sets: DefaultDict[str, List[Set[str]]] = defaultdict(list)
    for product in products:
        for allergen in product.allergens:
            allergen_to_ingredient_sets[allergen].append(product.ingredients)

    allergens_to_ingredient: Dict[str, str] = {}
    while len(allergens_to_ingredient) < len(allergen_to_ingredient_sets):
        for allergen, ingredient_sets in allergen_to_ingredient_sets.items():
            matching_ingredients = ingredient_sets[0].intersection(*ingredient_sets[1:])
            for ingredient in allergens_to_ingredient.values():
                if ingredient in matching_ingredients:
                    matching_ingredients.remove(ingredient)
            if len(matching_ingredients) == 1:
                allergens_to_ingredient[allergen] = list(matching_ingredients)[0]

    return allergens_to_ingredient


def puzzle1(input_file: Path):
    products = Product.products_from_file(input_file)
    allergens_to_ingredient = get_allergen_to_ingredient_mapping(products)
    return sum(
        cnt
        for ingredient, cnt in Product.all_ingredient_counts.items()
        if ingredient not in allergens_to_ingredient.values()
    )


def puzzle2(input_file: Path):
    products = Product.products_from_file(input_file)
    allergens_to_ingredient = get_allergen_to_ingredient_mapping(products)
    return ",".join(
        allergens_to_ingredient[allergen]
        for allergen in sorted(allergens_to_ingredient)
    )


if __name__ == "__main__":
    print("Day 21")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
