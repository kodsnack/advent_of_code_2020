from pathlib import Path
from typing import Tuple
import itertools


def _read_public_keys(input_file: Path) -> Tuple[int, int]:
    card, door = [int(v) for v in input_file.read_text().splitlines()]
    return card, door


def loop_size(public_key: int, subject_number: int) -> int:
    val = 1
    for possible_loop_size in itertools.count(start=1):
        val = (val * subject_number) % 20201227
        if val == public_key:
            break

    return possible_loop_size


def transform(subject_number: int, loop_size: int) -> int:
    val = 1
    for _ in range(loop_size):
        val = (val * subject_number) % 20201227
    return val


def puzzle1(input_file: Path):
    card, door = _read_public_keys(input_file)
    card_loop_size = loop_size(card, 7)
    return transform(door, card_loop_size)


def puzzle2(input_file: Path):
    return "Done"


if __name__ == "__main__":
    print("Day 25")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
