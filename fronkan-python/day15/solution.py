from typing import Dict, List


def determine_nth_number(starting_numbers: List[int], n: int):
    num_to_turn: Dict[int, int] = {
        num: turn for turn, num in enumerate(starting_numbers, start=1)
    }
    prev_number = starting_numbers[-1]
    for turn in range(len(starting_numbers) + 1, n + 1):
        last_turn = turn - 1
        if prev_number in num_to_turn:
            new_number = last_turn - num_to_turn[prev_number]
        else:
            new_number = 0
        num_to_turn[prev_number] = last_turn
        prev_number = new_number
    return prev_number


def puzzle1(starting_numbers: List[int]):
    return determine_nth_number(starting_numbers, 2020)


def puzzle2(starting_numbers: List[int]):
    return determine_nth_number(starting_numbers, 30000000)


if __name__ == "__main__":
    print("Day 15")
    starting_numbers = [0, 13, 16, 17, 1, 10, 6]
    print("Puzzle 1:", puzzle1(starting_numbers))
    print("Puzzle 2:", puzzle2(starting_numbers))
