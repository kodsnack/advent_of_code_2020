from pathlib import Path
from typing import Dict, List, Set, DefaultDict
from collections import Counter


def _read_adapter_jolts(input_file: Path) -> List[int]:
    with open(input_file) as f:
        return [int(line) for line in f]


def puzzle1(input_file: Path):
    adapter_jolts = sorted(_read_adapter_jolts(input_file))
    # Add outlet and my device
    adapter_jolts = [0, *adapter_jolts, max(adapter_jolts) + 3]
    jolt_diff_distribution = []
    for prev_idx, jolt in enumerate(adapter_jolts[1:]):
        prev_jolt = adapter_jolts[prev_idx]
        jolt_diff = jolt - prev_jolt
        jolt_diff_distribution.append(jolt_diff)

    counts = Counter(jolt_diff_distribution)
    return counts[1] * counts[3]


def puzzle2(input_file: Path):
    adapter_jolts = [0, *_read_adapter_jolts(input_file)]
    jolts_counts: Dict[int, int] = {j: 0 for j in adapter_jolts}
    jolts_counts[max(adapter_jolts) + 3] = 1
    for jolt in sorted(adapter_jolts, reverse=True):
        jolts_counts[jolt] = sum(
            [jolts_counts[j] for j in range(jolt + 1, jolt + 4) if j in jolts_counts]
        )

    return jolts_counts[0]


if __name__ == "__main__":
    print("Day 10")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
