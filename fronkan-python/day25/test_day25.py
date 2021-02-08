import importlib
import pytest
from pathlib import Path

DIRECTORY = Path(__file__).parent

solution = importlib.import_module(f"{DIRECTORY.name}.solution")


@pytest.mark.parametrize("public_key, loop_size", [(5764801, 8), (17807724, 11)])
def test_loop_size(public_key, loop_size):
    assert solution.loop_size(public_key, 7) == loop_size


@pytest.mark.parametrize(
    "puzzle, result", [(1, 14897079), (2, "Done")],
)
def test_puzzle_example_input(puzzle, result):
    input_file = Path(__file__).parent / "example_input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result


@pytest.mark.parametrize(
    "puzzle, result", [(1, 10548634), (2, "Done")],
)
def test_puzzle_real_input(puzzle, result):
    input_file = Path(__file__).parent / "input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result
