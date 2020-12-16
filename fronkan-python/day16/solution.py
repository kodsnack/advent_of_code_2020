from pathlib import Path
from typing import Callable, Dict, List
from aoc_lib.input_readers import read_chunk
from aoc_lib.maths import prod
from pprint import pprint


def _read_ticket_data(input_file: Path):
    ticket_specs, my_ticket, nearby_tickets = read_chunk(input_file)
    return ticket_specs, my_ticket[1], nearby_tickets[1:]


class Rule:
    def __init__(self, spec) -> None:
        self.spec = spec
        name, conditions = spec.split(":")
        self.name = name.strip()
        self.range1, self.range2 = [
            [int(val) for val in cond.split("-")] for cond in conditions.split(" or ")
        ]

    def __call__(self, val: int) -> bool:
        return (
            self.range1[0] <= val <= self.range1[1]
            or self.range2[0] <= val <= self.range2[1]
        )

    def __repr__(self) -> str:
        return f"Rule({self.spec})"

    def __eq__(self, o: object) -> bool:
        if not isinstance(o, Rule):
            return False
        return self.spec == o.spec


def puzzle1(input_file: Path):
    ticket_specs, _, nearby_tickets = _read_ticket_data(input_file)
    rules = [Rule(spec) for spec in ticket_specs]
    incorrect_values = []
    for ticket in nearby_tickets:
        for val in [int(v) for v in ticket.split(",")]:
            for rule in rules:
                if rule(val):
                    break
            else:
                incorrect_values.append(val)

    return sum(incorrect_values)


def _is_invalid_ticket(ticket: List[int], rules: List[Rule]):
    for val in ticket:
        if not any(rule(val) for rule in rules):
            return True
    return False


def parse_ticket(ticket: str) -> List[int]:
    return [int(v) for v in ticket.split(",")]


def puzzle2(input_file: Path):
    ticket_specs, my_ticket_str, nearby_tickets = _read_ticket_data(input_file)
    my_ticket = parse_ticket(my_ticket_str)
    rules = [Rule(spec) for spec in ticket_specs]
    tickets = [
        parsed_ticket
        for ticket in nearby_tickets
        if not _is_invalid_ticket(parsed_ticket := parse_ticket(ticket), rules)
    ]

    column_to_possible_rule = {idx: rules.copy() for idx in range(len(my_ticket))}
    for ticket in tickets:
        for idx, val in enumerate(ticket):
            to_delete = []
            for rule in column_to_possible_rule[idx]:
                if not rule(val):
                    to_delete.append(rule)
            for invalid_rule in to_delete:
                column_to_possible_rule[idx].remove(invalid_rule)

    final_selection: Dict[int, Rule] = {}
    while column_to_possible_rule:
        columns_to_delete = []
        for col, possible_rules in column_to_possible_rule.items():
            for determined_rule in final_selection.values():
                if determined_rule in possible_rules:
                    possible_rules.remove(determined_rule)

            if len(possible_rules) == 1:
                final_selection[col] = column_to_possible_rule[col][0]
                columns_to_delete.append(col)

        for col in columns_to_delete:
            del column_to_possible_rule[col]

    ticket_vals = [
        my_ticket[col]
        for col, rule in final_selection.items()
        if rule.name.startswith("departure")
    ]
    return prod(ticket_vals)


if __name__ == "__main__":
    print("Day 16")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
