import importlib
import pytest
from pathlib import Path

DIRECTORY = Path(__file__).parent

solution = importlib.import_module(f"{DIRECTORY.name}.solution")


@pytest.mark.parametrize(
    "puzzle, result", [(1, 25), (2, 286)],
)
def test_puzzle_example_input(puzzle, result):
    input_file = Path(__file__).parent / "example_input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result


@pytest.mark.parametrize(
    "puzzle, result", [(1, 796), (2, 39446)],
)
def test_puzzle_real_input(puzzle, result):
    input_file = Path(__file__).parent / "input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result
