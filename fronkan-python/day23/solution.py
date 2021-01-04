from pathlib import Path
from typing import List, Optional
from aoc_lib.maths import prod


class Node:
    __slots__ = ["val", "prev", "next"]

    def __init__(
        self, val: int, prev: Optional["Node"] = None, next: Optional["Node"] = None
    ) -> None:
        self.val = val
        self.prev = prev
        self.next = next

    def __repr__(self) -> str:
        return f"<Node(val={self.val}, prev={self.prev.val if self.prev else None}, next={self.next.val if self.next else None})>"

    def get_node_n_steps_forward(self, n: int):
        node = self
        for _ in range(n):
            node = node.next
        return node

    def get_next_n_values(self, n: int):
        node = self
        res = [node.val]
        for _ in range(n - 1):
            node = node.next
            res.append(node.val)
        return res


class CircularLinkedList:
    def __init__(self, items=None):
        self._val_to_node = {}
        self.start = Node(items[0], None, None)
        self._val_to_node[self.start.val] = self.start
        prev = self.start
        for item in items[1:]:
            new_node = Node(item, prev, None)
            self._val_to_node[new_node.val] = new_node
            prev.next = new_node
            prev = new_node
        self.end = prev
        self.start.prev = self.end
        self.end.next = self.start

    def __len__(self):
        return len(self._val_to_node)

    def __iter__(self):
        return self.iter_from(self.start)

    def iter_from(self, node):
        start_node = node
        yield start_node
        node = start_node.next
        while node != start_node:
            yield node
            node = node.next

    def find_by_value(self, val) -> Node:
        return self._val_to_node[val]

    def move_section(self, section_start: Node, section_end: Node, to: Node):
        # Remove section and mend hole
        # print(f"{section_start=}")
        # print(f"{section_end=}")
        # print(f"{to=}")
        prev_node = section_start.prev
        next_node = section_end.next
        prev_node.next = next_node
        next_node.prev = prev_node

        # Insert section
        section_start.prev = to
        section_end.next = to.next
        section_end.next.prev = section_end
        to.next = section_start

        # print("FINISHED")
        # print(f"{prev_node=}")
        # print(f"{next_node=}")
        # print(f"{section_start=}")
        # print(f"{section_end=}")
        # print(f"{to=}")


def read_input(input_file: Path) -> List[int]:
    return [int(c) for c in input_file.read_text().strip()]


def puzzle1(input_file: Path):
    cups = CircularLinkedList(read_input(input_file))
    current_cup = cups.start
    for _ in range(1, 100 + 1):
        pickup_start = current_cup.next
        lifted_cup_values = pickup_start.get_next_n_values(3)
        pickup_end = pickup_start.get_node_n_steps_forward(2)

        destination_cup_label = current_cup.val - 1
        while destination_cup_label in lifted_cup_values or destination_cup_label < 1:
            destination_cup_label -= 1
            if destination_cup_label < 1:
                destination_cup_label = len(cups)
        destination_node = cups.find_by_value(destination_cup_label)
        cups.move_section(pickup_start, pickup_end, destination_node)
        current_cup = current_cup.next

    return int("".join([str(i.val) for i in cups.iter_from(cups.find_by_value(1))][1:]))


def puzzle2(input_file: Path):
    input_numbers = read_input(input_file)
    cups = CircularLinkedList(
        [*input_numbers, *range(max(input_numbers) + 1, 1000000 + 1)]
    )
    current_cup = cups.start
    for _ in range(1, 10000000 + 1):
        pickup_start = current_cup.next
        lifted_cup_values = pickup_start.get_next_n_values(3)
        pickup_end = pickup_start.get_node_n_steps_forward(2)

        destination_cup_label = current_cup.val - 1
        while destination_cup_label in lifted_cup_values or destination_cup_label < 1:
            destination_cup_label -= 1
            if destination_cup_label < 1:
                destination_cup_label = len(cups)
        destination_node = cups.find_by_value(destination_cup_label)
        cups.move_section(pickup_start, pickup_end, destination_node)
        current_cup = current_cup.next

    return prod(cups.find_by_value(1).next.get_next_n_values(2))


if __name__ == "__main__":
    print("Day 23")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
