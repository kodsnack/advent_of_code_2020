from pathlib import Path
from typing import Dict, List


class Bag:
    name2bag: Dict[str, "Bag"] = {}

    def __init__(self, name: str, containment_info: Dict[str, int]) -> None:
        self.name = name
        self.containment_info = containment_info
        self.name2bag[self.name] = self

    def __hash__(self) -> int:
        return hash(self.name)

    @property
    def contained_bags(self) -> List["Bag"]:
        return [self.name2bag[name] for name in self.containment_info]

    def __repr__(self) -> str:
        type_name = type(self).__name__
        return f"{type_name}({self.name}, {self.containment_info})"

    def __contains__(self, bag_name: str) -> bool:
        if self.name == bag_name:
            return True
        elif not self.contained_bags:
            return False
        else:
            return any(bag_name in bag for bag in self.contained_bags)

    def count_bags(self) -> int:
        cnt = 0
        for name, amount in self.containment_info.items():
            cnt += amount + amount * self.name2bag[name].count_bags()
        return cnt


def _clean_bag_name(bag_name: str) -> str:
    cleaned_name = bag_name.strip()
    if cleaned_name.endswith("s"):
        cleaned_name = cleaned_name[:-1]
    return cleaned_name


def _read_packing_rules(input_file: Path) -> List[Bag]:
    bags = []
    with open(input_file) as f:
        for bag_rule in f:
            bag_rule = bag_rule.strip().strip(".")
            bag_name, contained_bag_name_and_counts = bag_rule.split(" contain ")
            bag_name = _clean_bag_name(bag_name)
            contained_bags = {}
            if contained_bag_name_and_counts.strip() != "no other bags":
                for bag_name_count in contained_bag_name_and_counts.split(","):
                    split_name_cnt = bag_name_count.split()
                    contained_cnt = int(split_name_cnt[0])
                    contained_name = _clean_bag_name(" ".join(split_name_cnt[1:]))
                    contained_bags[contained_name] = contained_cnt
            bags.append(Bag(bag_name, contained_bags))
    return bags


MY_BAG = "shiny gold bag"


def puzzle1(input_file: Path):
    bags = _read_packing_rules(input_file)
    cnt = 0
    for bag in bags:
        if bag.name == MY_BAG:
            continue
        if MY_BAG in bag:
            cnt += 1
    return cnt


def puzzle2(input_file: Path):
    _read_packing_rules(input_file)  # required to build up the bag structure
    return Bag.name2bag[MY_BAG].count_bags()


if __name__ == "__main__":
    print("Day 7")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
