from typing import Iterable
from itertools import combinations
from functools import reduce
import operator
from pathlib import Path
from aoc_lib.maths import prod


def _read_expense_report(expense_report_path):
    with open(expense_report_path) as _input:
        expenses = [int(expens) for expens in _input]
    return expenses


def _find_discrepancy(expenses: Iterable[int], combination_size: int) -> int:
    for combination in combinations(expenses, combination_size):
        if sum(combination) == 2020:
            return prod(combination)
    raise ValueError("No combination of expenses fulfil the criteria")


def puzzle1(expense_report_path):
    return _find_discrepancy(
        expenses=_read_expense_report(expense_report_path), combination_size=2
    )


def puzzle2(expense_report_path):
    return _find_discrepancy(
        expenses=_read_expense_report(expense_report_path), combination_size=3
    )


if __name__ == "__main__":
    print("Day 1")
    expense_report_path = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(expense_report_path))
    print("Puzzle 2:", puzzle2(expense_report_path))
