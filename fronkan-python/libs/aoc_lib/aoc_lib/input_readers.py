from typing import List
from pathlib import Path


def read_chunk(input_file: Path) -> List[List[str]]:
    chunks: List[List[str]] = []
    current_chunk: List[str] = []
    with open(input_file) as f:
        for line in f:
            line = line.strip()
            if not line and current_chunk:
                chunks.append(current_chunk)
                current_chunk = []
                continue
            current_chunk.append(line)
    if current_chunk:
        chunks.append(current_chunk)
    return chunks
