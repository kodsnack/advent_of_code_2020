from pathlib import Path
from typing import List


def _read_xmas_code(input_file: Path):
    with open(input_file) as f:
        xmas_code = [int(line.strip()) for line in f]
    return xmas_code


def _is_valid(number: int, previous_numbers: List[int]) -> bool:
    prev_num_lookup = set(previous_numbers)
    for prev_num in prev_num_lookup:
        companion_num = number - prev_num
        if companion_num in prev_num_lookup and companion_num != prev_num:
            return True
    return False


def _find_first_invalid_number(preamble, code):
    preamble_len = len(preamble)
    previous_numbers = list(reversed(preamble))
    for num in code:
        if not _is_valid(num, previous_numbers):
            return num
        previous_numbers.insert(0, num)
        previous_numbers = previous_numbers[:preamble_len]


def puzzle1(input_file: Path, preamble_len=25):
    xmas_code = _read_xmas_code(input_file)
    preamble, code = xmas_code[:preamble_len], xmas_code[preamble_len:]
    return _find_first_invalid_number(preamble, code)


def puzzle2(input_file: Path, preamble_len=25):
    xmas_code = _read_xmas_code(input_file)
    preamble, code = xmas_code[:preamble_len], xmas_code[preamble_len:]
    invalid_number = _find_first_invalid_number(preamble.copy(), code.copy())
    start = 0
    end = 1
    while end < len(xmas_code):
        code_range = xmas_code[start:end]
        code_sum = sum(code_range)
        if code_sum == invalid_number:
            return min(code_range) + max(code_range)

        if code_sum > invalid_number:
            start += 1
            end = start + 1
        else:
            end += 1


if __name__ == "__main__":
    print("Day 9")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
