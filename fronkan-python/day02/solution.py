from collections import Counter
from typing import Callable
from pathlib import Path


def _read_input(input_file):
    with open(input_file) as f:
        for line in f:
            num_range, dirty_char, pwd = line.split()
            num1, num2 = [int(num) for num in num_range.split("-")]
            char = dirty_char.rstrip(":")
            yield num1, num2, char, pwd


def _count_valid_passwords(
    input_file: Path, is_valid: Callable[[int, int, str, str], bool]
) -> int:
    correct_pwd_cnt = 0
    for num1, num2, char, pwd in _read_input(input_file):
        if is_valid(num1, num2, char, pwd):
            correct_pwd_cnt += 1
    return correct_pwd_cnt


def puzzle1(input_file):
    def _is_valid_pwd(min_cnt: int, max_cnt: int, char: str, pwd: str) -> bool:
        char_counts = Counter(pwd)
        return char_counts[char] >= min_cnt and char_counts[char] <= max_cnt

    return _count_valid_passwords(input_file, _is_valid_pwd)


def puzzle2(input_file):
    def _is_valid_pwd(idx1: int, idx2: int, char: str, pwd: str) -> bool:
        char1 = pwd[idx1 - 1]
        char2 = pwd[idx2 - 1]
        if char1 == char2:
            return False
        return char2 == char or char1 == char

    return _count_valid_passwords(input_file, _is_valid_pwd)


if __name__ == "__main__":
    print("Day 2")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
