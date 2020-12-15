from pathlib import Path
import os

for p in Path().iterdir():
    if p.is_dir() and p.name.startswith("day"):
        os.system(str(p / "solution.py"))
        print("-------------------------------------")
