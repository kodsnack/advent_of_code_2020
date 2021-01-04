import importlib
import pytest
from pathlib import Path

DIRECTORY = Path(__file__).parent

solution = importlib.import_module(f"{DIRECTORY.name}.solution")


@pytest.mark.parametrize(
    "puzzle, result", [(1, 112), (2, 848)],
)
def test_puzzle_example_input(puzzle, result):
    input_file = Path(__file__).parent / "example_input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result


@pytest.mark.parametrize(
    "puzzle, result", [(1, 368), (2, 2696)],
)
def test_puzzle_real_input(puzzle, result):
    input_file = Path(__file__).parent / "input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result
