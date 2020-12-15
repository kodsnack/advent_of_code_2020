from pathlib import Path
from typing import List
from aoc_lib.input_readers import read_chunk


def puzzle1(input_file: Path):
    sum_of_all_answer_counts = 0
    for group in read_chunk(input_file):
        unique_answers_in_group = {answer for person in group for answer in person}
        sum_of_all_answer_counts += len(unique_answers_in_group)
    return sum_of_all_answer_counts


def puzzle2(input_file: Path):
    sum_of_all_answer_counts = 0
    for group in read_chunk(input_file):
        answer_per_person = ({answer for answer in person} for person in group)
        answer_made_by_all_in_group = set.intersection(*answer_per_person)
        sum_of_all_answer_counts += len(answer_made_by_all_in_group)
    return sum_of_all_answer_counts


if __name__ == "__main__":
    print("Day 6")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
