from pathlib import Path
from aoc_lib.maths import prod


def _read_input(input_file: Path):
    map = []
    with open(input_file) as f:
        for line in f:
            row = list(line.strip())
            map.append(row)

    return map, len(row), len(map)


def _test_slope(x_step, y_step, map, num_cols, num_rows):
    x_max = num_cols
    x = 0
    y = 0
    tree_cnt = 0
    while y < num_rows - 1:
        x = (x + x_step) % (x_max)
        y += y_step
        if map[y][x] == "#":
            tree_cnt += 1
    return tree_cnt


def puzzle1(input_file: Path):
    tree_cnt = _test_slope(3, 1, *_read_input(input_file))
    return tree_cnt


def puzzle2(input_file: Path):
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    inputs = _read_input(input_file)
    tree_counts = [_test_slope(*slope, *inputs) for slope in slopes]
    return prod(tree_counts)


if __name__ == "__main__":
    print("Day 3")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
