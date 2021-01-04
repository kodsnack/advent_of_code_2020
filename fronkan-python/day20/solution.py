from pathlib import Path
from typing import Dict, List
import numpy as np
from aoc_lib.input_readers import read_chunk
from enum import Enum
from collections import namedtuple
import regex

Position = namedtuple("Position", ["row", "col"])


def image_to_string(image: np.ndarray) -> str:
    img_str = "".join(image.flatten())
    return img_str


class Edge(Enum):
    UP = "up"
    DOWN = "down"
    LEFT = "left"
    RIGHT = "right"

    def corresponding(self) -> "Edge":
        if self == Edge.UP:
            return Edge.DOWN
        elif self == Edge.DOWN:
            return Edge.UP
        elif self == Edge.LEFT:
            return Edge.RIGHT
        elif self == Edge.RIGHT:
            return Edge.LEFT
        else:
            raise ValueError()


class Tile:
    def __init__(self, tile_id: int, img: np.ndarray) -> None:
        self.tile_id = tile_id
        self.img = img
        self.original_img = img.copy()
        self.available_edges = [Edge.UP, Edge.DOWN, Edge.RIGHT, Edge.LEFT]

    def reset(self):
        self.img = self.original_img.copy()

    def take_edge(self, edge: Edge):
        self.available_edges.remove(edge)

    def rotate_once(self):
        self.img = np.rot90(self.img)

    def flip_top_bottom(self):
        self.img = np.flipud(self.img)

    def flip_left_right(self):
        self.img = np.fliplr(self.img)

    def get_edge(self, edge: Edge):
        if edge == edge.UP:
            return self.img[0, :]
        elif edge == edge.DOWN:
            return self.img[-1, :]
        elif edge == edge.LEFT:
            return self.img[:, 0]
        elif edge == edge.RIGHT:
            return self.img[:, -1]
        else:
            raise ValueError()

    def match(self, other: "Tile", edge: Edge):
        other_edge = edge.corresponding()
        return (self.get_edge(edge) == other.get_edge(other_edge)).all()

    def __repr__(self) -> str:
        return f"<Tile(tile_id={self.tile_id})>"

    def __eq__(self, other) -> bool:
        if isinstance(other, Tile):
            return self.tile_id == other.tile_id
        return False

    def __hash__(self) -> int:
        return hash(self.tile_id)


class Monster:
    def __init__(self, img: np.ndarray) -> None:
        self.img = img
        self.max_rows, self.max_cols = self.img.shape

    @property
    def length(self) -> int:
        return image_to_string(self.img).count("#")

    @classmethod
    def from_path(cls, path: Path) -> "Monster":
        with open(path) as f:
            data = [
                [char if char == "#" else "." for char in line if char != "\n"]
                for line in f
            ]
        return cls(np.array(data))

    def create_matcher_regex(self, img_width: int):
        padding = img_width - self.max_cols
        rows = ["".join(row) for row in self.img]
        return f".{{{padding}}}".join(rows)

    def count_occurrences_in_image(self, image: np.ndarray) -> int:
        _, img_width = image.shape
        matcher = self.create_matcher_regex(img_width)
        img_as_line = image_to_string(image)
        return len(regex.findall(matcher, img_as_line, overlapped=True))


def _read_input(input_file: Path) -> List[Tile]:
    res = []
    for chunk in read_chunk(input_file):
        img_id = int(regex.findall("[0-9]+", chunk[0])[0])
        img = np.array([list(line) for line in chunk[1:]])
        res.append(Tile(img_id, img))
    return res


def find_where_to_insert(tile: Tile, finished_tiles: List[Tile]):
    tile.reset()
    for f_tile in finished_tiles:
        for f_edge in f_tile.available_edges:

            for i in range(4):
                tile.rotate_once()
                if f_tile.match(tile, f_edge):
                    return f_tile, f_edge

            tile.flip_top_bottom()
            for i in range(4):
                tile.rotate_once()
                if f_tile.match(tile, f_edge):
                    return f_tile, f_edge

            tile.flip_left_right()
            for i in range(4):
                tile.rotate_once()
                if f_tile.match(tile, f_edge):
                    return f_tile, f_edge

    return None


def get_adjacent_in_direction(pos: Position, edge: Edge) -> Position:
    if edge == Edge.UP:
        return Position(row=pos.row - 1, col=pos.col)
    elif edge == Edge.DOWN:
        return Position(row=pos.row + 1, col=pos.col)
    elif edge == Edge.RIGHT:
        return Position(row=pos.row, col=pos.col + 1)
    elif edge == Edge.LEFT:
        return Position(row=pos.row, col=pos.col - 1)
    else:
        raise ValueError()


def build_image_puzzle(tiles: List[Tile]) -> Dict[Position, Tile]:
    puzzle: Dict[Tile, Position] = {tiles[0]: Position(0, 0)}
    tiles = tiles[1:]
    cnt = 1000
    while tiles:
        tile = tiles.pop(0)
        tile.reset()
        if (resp := find_where_to_insert(tile, list(puzzle.keys()))) :
            f_tile, f_edge = resp
            f_tile.take_edge(f_edge)
            tile.take_edge(f_edge.corresponding())
            pos = puzzle[f_tile]
            puzzle[tile] = get_adjacent_in_direction(pos, f_edge)
        else:
            tiles.append(tile)

        cnt -= 1
        if cnt <= 0:
            print("tiles left:", tiles)
            raise RuntimeError("Max iterations")

    pos2tile = {
        pos: tile for tile, pos in sorted(puzzle.items(), key=lambda item: item[1])
    }
    return pos2tile


def puzzle1(input_file: Path):
    tiles = _read_input(input_file)
    pos2tile = build_image_puzzle(tiles)
    prev_row = list(pos2tile.keys())[0].row
    rows = []
    row = []
    for pos, tile in pos2tile.items():
        if pos.row != prev_row:
            prev_row = pos.row
            rows.append(row)
            row = []
        row.append(tile.tile_id)
    rows.append(row)
    return rows[0][0] * rows[0][-1] * rows[-1][0] * rows[-1][-1]


def puzzle2(input_file: Path):
    tiles = _read_input(input_file)
    pos2tile = build_image_puzzle(tiles)
    prev_row = list(pos2tile.keys())[0].row
    rows = []
    row = []
    for pos, tile in pos2tile.items():
        if pos.row != prev_row:
            prev_row = pos.row
            rows.append(row)
            row = []
        row.append(tile.img[1:-1, 1:-1])
    rows.append(row)
    finished_img = np.concatenate([np.concatenate(row, axis=1) for row in rows], axis=0)
    monster = Monster.from_path(input_file.parent / "monster.txt")

    for flipper in [lambda x: x, np.fliplr, np.flipud]:
        img = flipper(finished_img.copy())
        for i in range(4):
            img = np.rot90(img)
            if cnt := monster.count_occurrences_in_image(img):
                return image_to_string(img).count("#") - cnt * monster.length


if __name__ == "__main__":
    print("Day 20")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))

