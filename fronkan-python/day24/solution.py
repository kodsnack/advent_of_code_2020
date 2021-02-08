from pathlib import Path
from typing import Generator, List, Set, Tuple
import numpy as np

# Point: (y, x, z)
# According to this way of mapping hexagons to cubes:
# https://www.redblobgames.com/grids/hexagons/#coordinates-cube
INSTRUCTION_TO_VECTOR = {
    "e": np.array((-1, 1, 0)),
    "se": np.array((-1, 0, 1)),
    "sw": np.array((0, -1, 1)),
    "w": np.array((1, -1, 0)),
    "nw": np.array((1, 0, -1)),
    "ne": np.array((0, 1, -1)),
}

HashableTile = Tuple[int, int, int]


def _read_tile_instructions(input_file: Path) -> Generator[str, None, None]:
    with open(input_file, "r") as f:
        for line in f:
            cleaned_line = line.strip()
            if cleaned_line:
                yield cleaned_line
            else:
                raise RuntimeError()


def _directions(tile_instruction: str) -> Generator[str, None, None]:
    chars = list(tile_instruction)
    while len(chars) > 0:
        if chars[0] in ["e", "w"]:
            instr = chars[0]
            chars = chars[1:]
        else:
            instr = "".join(chars[:2])
            chars = chars[2:]
        yield instr


def initial_black_tiles(input_file: Path) -> Set[HashableTile]:
    black_tiles: Set[HashableTile] = set()
    for tile_instr in _read_tile_instructions(input_file):
        pos = np.array((0, 0, 0))
        for instr in _directions(tile_instr):
            pos += INSTRUCTION_TO_VECTOR[instr]

        hashable_tile: HashableTile = tuple(pos)
        if hashable_tile in black_tiles:
            black_tiles.remove(hashable_tile)
        else:
            black_tiles.add(hashable_tile)
    return black_tiles


def puzzle1(input_file: Path):
    black_tiles = initial_black_tiles(input_file)
    return len(black_tiles)


def adjacent_tiles(hashable_tile: HashableTile) -> List[HashableTile]:
    tile = np.array(hashable_tile)
    return [tuple(tile + direction) for direction in INSTRUCTION_TO_VECTOR.values()]


def check_white_to_black(hashable_tile: HashableTile, black_tiles: Set[HashableTile]):
    cnt = sum([1 if ht in black_tiles else 0 for ht in adjacent_tiles(hashable_tile)])
    return cnt == 2


def check_black_to_white(hashable_tile: HashableTile, black_tiles: Set[HashableTile]):
    cnt = sum([1 if ht in black_tiles else 0 for ht in adjacent_tiles(hashable_tile)])
    return cnt == 0 or cnt > 2


def puzzle2(input_file: Path):
    black_tiles = initial_black_tiles(input_file)
    for _ in range(100):
        checked_tiles = set()
        to_remove = []
        to_add = []
        for bt in black_tiles:
            checked_tiles.add(bt)
            if check_black_to_white(bt, black_tiles):
                to_remove.append(bt)
            for t in adjacent_tiles(bt):
                if t not in black_tiles and t not in checked_tiles:
                    checked_tiles.add(t)
                    if check_white_to_black(t, black_tiles):
                        to_add.append(t)
        for t_to_remove in to_remove:
            black_tiles.remove(t_to_remove)
        for t_to_add in to_add:
            black_tiles.add(t_to_add)
    return len(black_tiles)


if __name__ == "__main__":
    print("Day 24")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
