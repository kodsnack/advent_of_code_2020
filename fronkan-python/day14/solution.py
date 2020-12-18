from pathlib import Path
from collections import defaultdict
from typing import DefaultDict, Dict


class Decoder:
    def __init__(self, mask: str) -> None:
        self.memory: DefaultDict[int, int] = defaultdict(int)
        self.mask = mask

    @property
    def mask(self):
        return self._mask

    @mask.setter
    def mask(self, mask: str):
        self._mask = mask
        self._mask_dict = self._create_mask_lookup(mask)

    @staticmethod
    def _create_mask_lookup(mask):
        return {idx: bit for idx, bit in enumerate(mask) if bit != "X"}

    def update_memory(self, mem_idx: int, value: int):
        bits = list(f"{value:036b}")
        for mask_idx, bit in self._mask_dict.items():
            bits[mask_idx] = bit
        self.memory[mem_idx] = int("".join(bits), 2)


def _read_input(input_file):
    instructions = []
    with open(input_file) as f:
        for line in f:
            var, val = line.split("=")
            instructions.append((var.strip(), val.strip()))
    return instructions


def puzzle1(input_file: Path):
    instructions = _read_input(input_file)
    init_mask = instructions[0][1]
    computer = Decoder(init_mask)
    for var, val in instructions[1:]:
        if var.startswith("mem"):
            mem_idx = int(var[4:-1])
            computer.update_memory(mem_idx, int(val))
        elif var == "mask":
            computer.mask = val
        else:
            raise ValueError(f"Invalid instruction: {var} - {val}")
    return sum(computer.memory.values())


class DecoderV2(Decoder):
    @staticmethod
    def _create_mask_lookup(mask):
        return {idx: bit for idx, bit in enumerate(mask) if bit != "0"}

    def update_memory(self, mem_idx: int, value: int):
        idx_bits = list(f"{mem_idx:036b}")
        for mask_idx, bit in self._mask_dict.items():
            if bit == "1":
                idx_bits[mask_idx] = "1"

        mem_indices = [idx_bits]
        for mask_idx, bit in self._mask_dict.items():
            if bit == "X":
                indices_to_add = []
                for idx_as_bits in mem_indices:
                    new_idx = idx_as_bits.copy()
                    new_idx[mask_idx] = "0" if new_idx[mask_idx] == "1" else "1"
                    indices_to_add.append(new_idx)
                mem_indices.extend(indices_to_add)

        for idx_as_bits in mem_indices:
            self.memory[int("".join(idx_as_bits), 2)] = value


def puzzle2(input_file: Path):
    instructions = _read_input(input_file)
    init_mask = instructions[0][1]
    computer = DecoderV2(init_mask)
    for var, val in instructions[1:]:
        if var.startswith("mem"):
            mem_idx = int(var[4:-1])
            computer.update_memory(mem_idx, int(val))
        elif var == "mask":
            computer.mask = val
        else:
            raise ValueError(f"Invalid instruction: {var} - {val}")
    return sum(computer.memory.values())


if __name__ == "__main__":
    print("Day 14")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
