from pathlib import Path
from typing import Dict, List, Set, Tuple
from math import ceil, gcd


def _read_buss_data(input_file: Path) -> Tuple[int, List[str]]:
    earliest_time, busses_str = input_file.read_text().splitlines()
    return int(earliest_time), busses_str.split(",")


def puzzle1(input_file: Path):
    earliest_time, busses = _read_buss_data(input_file)
    wait_time_for_buss: Dict[int, int] = {}
    for buss in busses:
        if buss == "x":
            continue
        buss_id = int(buss)
        closest_time = ceil(earliest_time / buss_id) * buss_id
        wait_time = closest_time - earliest_time
        wait_time_for_buss[wait_time] = buss_id

    shortest_time = min(wait_time_for_buss)
    return shortest_time * wait_time_for_buss[shortest_time]


def puzzle2(input_file: Path):
    _, busses = _read_buss_data(input_file)
    id_and_t_offset_pairs = [
        (idx, int(buss)) for idx, buss in enumerate(busses) if buss != "x"
    ]

    t = 0
    step_size = 1
    found_buss_ids: Set[int] = set()
    while True:
        t += step_size
        for idx, buss in id_and_t_offset_pairs:
            if (t + idx) % buss != 0:
                break
            elif buss not in found_buss_ids:
                found_buss_ids.add(buss)
                step_size = (buss * step_size) // gcd(buss, step_size)
        else:
            break

    return t


if __name__ == "__main__":
    print("Day 13")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
