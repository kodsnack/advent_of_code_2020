from pathlib import Path
from collections import namedtuple
from typing import List, Tuple

Degree = int  # 0 -> 360


class GridMovable:
    def __init__(self, x: int, y: int) -> None:
        self.x = x
        self.y = y

    def north(self, val: int):
        self.y += val

    def south(self, val: int):
        self.y -= val

    def east(self, val: int):
        self.x += val

    def west(self, val: int):
        self.x -= val


class Ship(GridMovable):
    def __init__(self) -> None:
        super().__init__(0, 0)
        self._direction_idx = 1
        self._directions = [self.north, self.east, self.south, self.west]

    def rotate(self, degree: Degree):
        num_directions = 4
        idx_change = (degree // 90) % num_directions
        new_idx = (self._direction_idx + idx_change) % num_directions
        self._direction_idx = new_idx

    def right(self, degree: int):
        self.rotate(degree)

    def left(self, degree: int):
        self.rotate(-degree)

    def forward(self, val: int):
        self._directions[self._direction_idx](val)


def _read_ship_instructions(input_file: Path) -> List[Tuple[str, int]]:
    with open(input_file) as f:
        return [
            (cleaned_line[0], int(cleaned_line[1:]))
            for line in f
            if (cleaned_line := line.strip())
        ]


def puzzle1(input_file: Path):
    ship = Ship()
    translations = {
        "R": ship.right,
        "L": ship.left,
        "N": ship.north,
        "S": ship.south,
        "W": ship.west,
        "E": ship.east,
        "F": ship.forward,
    }
    instructions = _read_ship_instructions(input_file)
    for direction, value in instructions:
        fkn = translations[direction]
        fkn(value)

    manhattan = abs(ship.x) + abs(ship.y)
    return manhattan


class Waypoint(GridMovable):
    def __init__(self) -> None:
        super().__init__(10, 1)

    def left(self, degree: Degree):
        steps = degree // 90
        pos = (self.x, self.y)
        for i in range(steps):
            pos = (-pos[1], pos[0])
        self.x = pos[0]
        self.y = pos[1]

    def right(self, degree: Degree):
        steps = degree // 90
        pos = (self.x, self.y)
        for i in range(steps):
            pos = (pos[1], -pos[0])
        self.x = pos[0]
        self.y = pos[1]

    def forward(self, val: int):
        raise NotImplementedError()


class ShipV2:
    def __init__(self) -> None:
        self.x = 0
        self.y = 0

    def forward(self, waypoint: Waypoint, steps: int):
        self.x += waypoint.x * steps
        self.y += waypoint.y * steps


def puzzle2(input_file: Path):
    ship = ShipV2()
    waypoint = Waypoint()
    translations = {
        "R": waypoint.right,
        "L": waypoint.left,
        "N": waypoint.north,
        "S": waypoint.south,
        "W": waypoint.west,
        "E": waypoint.east,
    }
    instructions = _read_ship_instructions(input_file)
    for direction, value in instructions:
        if direction == "F":
            ship.forward(waypoint, value)
        else:
            fkn = translations[direction]
            fkn(value)

    manhattan = abs(ship.x) + abs(ship.y)
    return manhattan


if __name__ == "__main__":
    print("Day 12")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
