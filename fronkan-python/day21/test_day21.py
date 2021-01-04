import importlib
import pytest
from pathlib import Path

DIRECTORY = Path(__file__).parent

solution = importlib.import_module(f"{DIRECTORY.name}.solution")
Product = getattr(solution, "Product")


@pytest.mark.parametrize(
    "puzzle, result", [(1, 5), (2, "mxmxvkd,sqjhc,fvjkl")],
)
def test_puzzle_example_input(puzzle, result):
    Product.reset_cls_counters()
    input_file = Path(__file__).parent / "example_input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result


@pytest.mark.parametrize(
    "puzzle, result",
    [(1, 1885), (2, "fllssz,kgbzf,zcdcdf,pzmg,kpsdtv,fvvrc,dqbjj,qpxhfp")],
)
def test_puzzle_real_input(puzzle, result):
    Product.reset_cls_counters()
    input_file = Path(__file__).parent / "input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result
