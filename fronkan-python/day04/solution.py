from typing import Dict, List
from pathlib import Path
from string import digits, ascii_lowercase
from functools import partial


def _read_passports(input_file: Path) -> List[Dict[str, str]]:
    passports = []
    with open(input_file) as f:
        passport = {}
        for line in f:
            if not line.strip():
                passports.append(passport)
                passport = {}
                continue
            tokens = line.strip().split()
            field_value_pairs = [tuple(token.split(":")) for token in tokens]
            passport.update(dict(field_value_pairs))

        # Add the passport as there is no blank line in the end
        if passport:
            passports.append(passport)

    return passports


def puzzle1(input_file: Path):
    PASSPORT_FIELDS = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"}

    def _is_valid(passport):
        field_diff = list(PASSPORT_FIELDS - set(passport.keys()))
        if len(field_diff) == 1 and field_diff[0] == "cid":
            return True
        return not field_diff

    valid_passports = [
        passport for passport in _read_passports(input_file) if _is_valid(passport)
    ]
    return len(valid_passports)


def validate_year(val: str, min_year: int, max_year: int):
    return len(val) == 4 and min_year <= int(val) <= max_year


def validate_height(hgt: str):
    if hgt.endswith("cm"):
        return 150 <= int(hgt[:-2]) <= 193
    if hgt.endswith("in"):
        return 59 <= int(hgt[:-2]) <= 76
    return False


def validate_color(hcl: str):
    valid_chars = set([*digits, *ascii_lowercase])
    if hcl.startswith("#") and len(hcl) == 7:
        return all(char in valid_chars for char in hcl[1:])
    return False


def validate_eye_color(ecl: str):
    return ecl in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


def validate_pid(pid: str):
    return len(pid) == 9


def puzzle2(input_file: Path):
    validators = {
        "byr": partial(validate_year, min_year=1920, max_year=2002),
        "iyr": partial(validate_year, min_year=2010, max_year=2020),
        "eyr": partial(validate_year, min_year=2020, max_year=2030),
        "hgt": validate_height,
        "hcl": validate_color,
        "ecl": validate_eye_color,
        "pid": validate_pid,
    }

    def _is_valid(passport):
        for validator_key, validator in validators.items():
            if validator_key not in passport:
                return False
            if not validator(passport[validator_key]):
                return False
        return True

    valid_passports = [
        passport for passport in _read_passports(input_file) if _is_valid(passport)
    ]
    return len(valid_passports)


if __name__ == "__main__":
    print("Day 4")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
