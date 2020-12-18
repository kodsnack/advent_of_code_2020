from pathlib import Path
from typing import Iterable, List
from collections import namedtuple
from math import sqrt, ceil

Point = namedtuple("Point", ["col", "row"])


class Map:
    def __init__(self, map: List[List[str]]) -> None:
        self.map = map
        self.row_cnt = len(self.map)
        self.col_cnt = len(self.map[0])
        self.diag_len = ceil(sqrt(self.row_cnt ** 2 + self.col_cnt ** 2))

    @classmethod
    def from_file(cls, input_file: Path) -> "Map":
        map = []
        with open(input_file) as f:
            for row in f:
                map.append([char for char in row.strip()])
        return cls(map)

    def neighboring_positions(self, point: Point) -> List[str]:
        return [
            self.map[row][col]
            for row in range(point.row - 1, point.row + 2)
            for col in range(point.col - 1, point.col + 2)
            if (
                0 <= col < self.col_cnt
                and 0 <= row < self.row_cnt
                and ((col, row) != point)
            )
        ]

    def seats_in_sight(self, point: Point) -> List[str]:
        directions = [
            (1, 0),
            (-1, 0),
            (0, 1),
            (0, -1),
            (1, 1),
            (1, -1),
            (-1, 1),
            (-1, -1),
        ]
        seats = []
        for col_step, row_step in directions:
            col = point.col + col_step
            row = point.row + row_step
            while 0 <= col < self.col_cnt and 0 <= row < self.row_cnt:
                if self.map[row][col] != ".":
                    seats.append(self.map[row][col])
                    break
                col += col_step
                row += row_step
        return seats

    def value(self, point: Point) -> str:
        return self.map[point.row][point.col]

    def set_value(self, point: Point, val: str):
        if self.value(point) == ".":
            raise ValueError("Floor can't change")
        self.map[point.row][point.col] = val

    @property
    def all_points(self) -> List[Point]:
        return [
            Point(col, row)
            for col in range(self.col_cnt)
            for row in range(self.row_cnt)
            if self.value(Point(col, row)) != "."
        ]

    @property
    def occupied_cnt(self):
        return sum(
            [
                1
                for col in range(self.col_cnt)
                for row in range(self.row_cnt)
                if self.map[row][col] == "#"
            ]
        )


def all_neighbors_are_empty(neighbors):
    return len([n for n in neighbors if n == "#"]) == 0


def is_too_crowded(neighbors, too_many):
    return len([n for n in neighbors if n == "#"]) >= too_many


def puzzle1(input_file: Path):
    map = Map.from_file(input_file)
    while True:
        updates = {}
        for point in map.all_points:
            seat = map.value(point)
            neighbors = map.neighboring_positions(point)
            if seat == "L" and all_neighbors_are_empty(neighbors):
                updates[point] = "#"
            elif seat == "#" and is_too_crowded(neighbors, 4):
                updates[point] = "L"
        if not updates:
            break

        for p, v in updates.items():
            map.set_value(p, v)
    return map.occupied_cnt


def puzzle2(input_file: Path):
    map = Map.from_file(input_file)
    while True:
        updates = {}
        for point in map.all_points:
            seat = map.value(point)
            neighbors = map.seats_in_sight(point)
            if seat == "L" and all_neighbors_are_empty(neighbors):
                updates[point] = "#"
            elif seat == "#" and is_too_crowded(neighbors, 5):
                updates[point] = "L"

        if not updates:
            break

        for p, v in updates.items():
            map.set_value(p, v)
    return map.occupied_cnt


if __name__ == "__main__":
    print("Day 11")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
