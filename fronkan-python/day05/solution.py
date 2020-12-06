from pathlib import Path
from typing import List, Tuple
from itertools import product


class Seat:
    def __init__(self, row: int, col: int) -> None:
        self.row = row
        self.col = col

    @classmethod
    def from_binary_seat(cls, binary_seat: str):
        binary_seat = binary_seat
        row_bits = binary_seat[:7]
        col_bits = binary_seat[7:]
        row = cls._parse_bits(row_bits, 128, "F", "B")
        col = cls._parse_bits(col_bits, 8, "L", "R")
        return cls(row, col)

    @staticmethod
    def _parse_bits(
        bits: str, num_positions: int, lower_bit: str, upper_bit: str
    ) -> int:
        pos_group = range(num_positions)
        for bit in bits:
            new_group_length = len(pos_group) // 2
            if bit == lower_bit:
                pos_group = pos_group[:new_group_length]
            elif bit == upper_bit:
                pos_group = pos_group[new_group_length:]
            else:
                raise ValueError(f"Unknown bit: {bit}")
        assert len(pos_group) == 1
        return pos_group[0]

    @property
    def seat_id(self) -> int:
        return self.row * 8 + self.col

    def __hash__(self) -> int:
        return hash((self.row, self.col))

    def __eq__(self, o: object) -> bool:
        if isinstance(o, type(self)):
            return (self.row, self.col) == (o.row, o.col)
        return False

    def __lt__(self, o: object) -> bool:
        if isinstance(o, type(self)):
            return (self.row, self.col) < (o.row, o.col)
        raise ValueError(
            f"Can not compare {type(self).__name__} with {type(o).__name__}"
        )


def _read_seats(input_file: Path) -> List[Seat]:
    with open(input_file) as f:
        seats = [Seat.from_binary_seat(row.strip()) for row in f if row.strip()]
    return seats


def puzzle1(input_file: Path):
    seats = _read_seats(input_file)
    return max(seat.seat_id for seat in seats)


def puzzle2(input_file: Path):
    seats = _read_seats(input_file)
    all_seats = {Seat(*pos) for pos in product(range(128), range(7))}
    possible_seats = all_seats - set(seats)
    seat_ids = {seat.seat_id for seat in seats}

    for seat in possible_seats:
        if (seat.seat_id - 1) in seat_ids and (seat.seat_id + 1) in seat_ids:
            return seat.seat_id


if __name__ == "__main__":
    print("Day 5")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
