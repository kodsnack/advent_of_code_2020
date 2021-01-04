from pathlib import Path
from typing import Iterable, List, Set, Sequence, Tuple, Union
from collections import namedtuple
from itertools import product

Point3D = namedtuple("Point", ["row", "col", "layer"])
Point4D = namedtuple("Point", ["row", "col", "layer", "fourth_wall"])

Point = Union[Point3D, Point4D]


def neighboring_points(p: Point):
    dims = len(p)
    yield from (
        type(p)(*(sum(vals) for vals in zip(p, direction)))
        for direction in product([1, 0, -1], repeat=dims)
        if direction != (0,) * dims
    )


def count_active(point, active_nodes: Set[Point]) -> int:
    return len([n for n in neighboring_points(point) if n in active_nodes])


def _read_active_conway_cubes(input_file: Path) -> Iterable[Tuple[int, int]]:
    with open(input_file) as f:
        for row, line in enumerate(f):
            for col, val in enumerate(line):
                if val == "#":
                    yield (row, col)


def run_cycles(active_nodes, cycles=6):
    active_nodes = active_nodes.copy()
    for cycle in range(cycles):
        nodes_to_deactivate = []
        inactive_neighbors = set()
        for active_node in active_nodes:
            active_cnt = 0
            for neighbor in neighboring_points(active_node):
                if neighbor in active_nodes:
                    active_cnt += 1
                else:
                    inactive_neighbors.add(neighbor)
            if not (2 <= active_cnt <= 3):
                nodes_to_deactivate.append(active_node)

        nodes_to_activate = {
            inactive_node
            for inactive_node in inactive_neighbors
            if count_active(inactive_node, active_nodes) == 3
        }
        for n in nodes_to_deactivate:
            active_nodes.remove(n)
        for n in nodes_to_activate:
            active_nodes.add(n)
    return active_nodes


def puzzle1(input_file: Path):
    active_nodes = set(Point3D(*c, 0) for c in _read_active_conway_cubes(input_file))
    active_nodes = run_cycles(active_nodes, 6)
    return len(active_nodes)


def puzzle2(input_file: Path):
    active_nodes = set(Point4D(*c, 0, 0) for c in _read_active_conway_cubes(input_file))
    active_nodes = run_cycles(active_nodes, 6)
    return len(active_nodes)


if __name__ == "__main__":
    print("Day 17")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
