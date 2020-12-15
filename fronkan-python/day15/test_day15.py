import importlib
import pytest
from pathlib import Path

DIRECTORY = Path(__file__).parent

solution = importlib.import_module(f"{DIRECTORY.name}.solution")


@pytest.mark.parametrize(
    "data, result",
    [
        ([0, 3, 6], 436),
        ([1, 3, 2], 1),
        ([2, 1, 3], 10),
        ([1, 2, 3], 27),
        ([2, 3, 1], 78),
        ([3, 2, 1], 438),
        ([3, 1, 2], 1836),
    ],
)
def test_puzzle1_example_input(data, result):
    puzzle_solver = getattr(solution, f"puzzle1")
    assert puzzle_solver(data) == result


@pytest.mark.slow
@pytest.mark.parametrize(
    "data, result",
    [
        ([0, 3, 6], 175594),
        ([1, 3, 2], 2578),
        ([2, 1, 3], 3544142),
        ([1, 2, 3], 261214),
        ([2, 3, 1], 6895259),
        ([3, 2, 1], 18),
        ([3, 1, 2], 362),
    ],
)
def test_puzzle2_example_input(data, result):
    puzzle_solver = getattr(solution, f"puzzle2")
    assert puzzle_solver(data) == result


@pytest.mark.parametrize(
    "puzzle, result", [(1, 276), (2, 31916)],
)
def test_puzzle_real_input(puzzle, result):
    data = [0, 13, 16, 17, 1, 10, 6]
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(data) == result

