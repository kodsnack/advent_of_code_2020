from typing import Iterable
from itertools import combinations
from functools import reduce
import operator


def _read_expense_report():
    with open("input.txt") as _input:
        expenses = [int(expens) for expens in _input]
    return expenses


def _prod(nums: Iterable[int]) -> int:
    return reduce(operator.mul, nums)


def _find_discrepancy(expenses: Iterable[int], combination_size: int) -> int:
    for combination in combinations(expenses, combination_size):
        if sum(combination) == 2020:
            return _prod(combination)
    raise ValueError("No combination of expenses fulfil the criteria")


def puzzle1():
    print(
        "Puzzle 1:",
        _find_discrepancy(expenses=_read_expense_report(), combination_size=2),
    )


def puzzle2():
    print(
        "Puzzle 2:",
        _find_discrepancy(expenses=_read_expense_report(), combination_size=3),
    )


if __name__ == "__main__":
    puzzle1()
    puzzle2()
