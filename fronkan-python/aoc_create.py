from pathlib import Path
from datetime import date
from argparse import ArgumentParser

# --------------------------------------- PARSER ---------------------------------------
parser = ArgumentParser()
parser.add_argument(
    "--day",
    "-d",
    type=int,
    default=date.today().day,
    help="Day for which a folder should be created [default = today]",
)

args = parser.parse_args()

DAY_TO_GENERATE = args.day

# --------------------------------------- TEMPLATES ---------------------------------------

TEST_FILE_CONTENT = """
import importlib
import pytest
from pathlib import Path

DIRECTORY = Path(__file__).parent

solution = importlib.import_module(f"{DIRECTORY.name}.solution")


@pytest.mark.parametrize("puzzle, result", [(1, None), (2, None)])
def test_puzzle_example_input(puzzle, result):
    input_file = Path(__file__).parent / "example_input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result


@pytest.mark.parametrize("puzzle, result", [(1, None), (2, None)])
def test_puzzle_real_input(puzzle, result):
    input_file = Path(__file__).parent / "input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result
"""
SOLUTION_FILE_CONTENT = f"""
from pathlib import Path


def puzzle1(input_file):
    raise NotImplementedError()



def puzzle2(input_file):
    raise NotImplementedError()


if __name__ == "__main__":
    print("Day {DAY_TO_GENERATE}")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
"""

# --------------------------------------- GENERATE FILES ---------------------------------------
file_name = (
    f"day0{DAY_TO_GENERATE}" if DAY_TO_GENERATE < 10 else f"day{DAY_TO_GENERATE}"
)
new_aoc_folder = Path() / file_name
if new_aoc_folder.exists():
    print(f"Directory for day {DAY_TO_GENERATE} already exists")
    exit(-1)

new_aoc_folder.mkdir()
(new_aoc_folder / "input.txt").touch()
(new_aoc_folder / "example_input.txt").touch()

with open(new_aoc_folder / "solution.py", "w") as f:
    f.write(SOLUTION_FILE_CONTENT.strip())

with open(new_aoc_folder / f"test_day{DAY_TO_GENERATE}.py", "w") as f:
    f.write(TEST_FILE_CONTENT.strip())
