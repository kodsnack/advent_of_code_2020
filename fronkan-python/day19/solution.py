from pathlib import Path
from aoc_lib.input_readers import read_chunk
import re


def re_transpiler(rule_spec, unbuilt_rules_specs, compiled_rules=None):
    if compiled_rules is None:
        compiled_rules = {}
    if "|" in rule_spec:
        left, right = (
            re_transpiler(sub_rule, unbuilt_rules_specs, compiled_rules)
            for sub_rule in rule_spec.split("|")
        )
        return f"(?:{left}|{right})"
    elif '"' in rule_spec:
        return rule_spec.strip().strip('"')
    else:
        rules = []
        for rule_idx in rule_spec.split():
            rule_idx = int(rule_idx)
            if rule_idx in compiled_rules:
                built_rule = compiled_rules[rule_idx]
            else:
                built_rule = re_transpiler(
                    unbuilt_rules_specs[rule_idx], unbuilt_rules_specs, compiled_rules,
                )
                compiled_rules[rule_idx] = built_rule
            rules.append(built_rule)
        return "".join(rules)


def puzzle1(input_file: Path):
    rule_specs, messages = read_chunk(input_file)
    rules_specs = {}
    for rule_spec in rule_specs:
        rule_id, spec = rule_spec.split(":")
        rules_specs[int(rule_id)] = spec.strip()
    rule = re_transpiler(rules_specs[0], rules_specs)
    cnt = 0
    for message in messages:
        if re.fullmatch(rule, message) is not None:
            cnt += 1
    return cnt


def puzzle2(input_file: Path):
    rule_specs, messages = read_chunk(input_file)
    rule_id_to_spec = {}
    for rule_spec in rule_specs:
        rule_id, spec = rule_spec.split(":")
        rule_id_to_spec[int(rule_id)] = spec.strip()

    pre_compiled = {}
    rule_42 = re_transpiler(rule_id_to_spec[42], rule_id_to_spec)
    pre_compiled[42] = rule_42
    pre_compiled[8] = f"{rule_42}+"
    rule_31 = re_transpiler(rule_id_to_spec[31], rule_id_to_spec)
    pre_compiled[31] = rule_31
    rule_11_ors = "|".join(f"{rule_42}{{{i}}}{rule_31}{{{i}}}" for i in range(1, 72))
    pre_compiled[11] = f"(?:{rule_11_ors})"
    rule = re_transpiler(rule_id_to_spec[0], rule_id_to_spec, pre_compiled)
    cnt = 0
    for message in messages:
        if re.fullmatch(rule, message) is not None:
            cnt += 1
    return cnt


if __name__ == "__main__":
    print("Day 19")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
