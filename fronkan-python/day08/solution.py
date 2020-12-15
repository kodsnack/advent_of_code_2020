from pathlib import Path
from typing import List, Optional, Tuple, Set
from collections import namedtuple
from abc import abstractmethod, ABC

PCState = namedtuple("PCState", ["accumulator", "pc"])


class Operation(ABC):
    def __init__(self, val: int) -> None:
        self.val = val

    @staticmethod
    def _parse_line(line: str) -> Tuple[str, int]:
        operation, val = line.split()
        operation = operation.strip()
        return (operation, int(val))

    @staticmethod
    def from_line(line: str) -> "Operation":
        op, val = Operation._parse_line(line)
        if op == "acc":
            return Acc(val)
        elif op == "nop":
            return Nop(val)
        elif op == "jmp":
            return Jmp(val)
        else:
            raise ValueError(f"Unsupported operation: '{op}'")

    @abstractmethod
    def execute(self, state: PCState):
        raise NotImplementedError()


class Acc(Operation):
    def execute(self, state: PCState):
        return PCState(state.accumulator + self.val, state.pc + 1)


class Nop(Operation):
    def execute(self, state: PCState):
        return PCState(state.accumulator, state.pc + 1)


class Jmp(Operation):
    def execute(self, state: PCState):
        return PCState(state.accumulator, state.pc + self.val)


def _read_program(input_file: Path):
    instructions: List[Operation] = []
    with open(input_file) as f:
        for line in f:
            instructions.append(Operation.from_line(line))
    return instructions


def puzzle1(input_file: Path):
    state = PCState(0, 0)
    program = _read_program(input_file)
    executed_instruction = set()
    while state.pc not in executed_instruction:
        executed_instruction.add(state.pc)
        instruction = program[state.pc]
        state = instruction.execute(state)

    return state.accumulator


def flip_flop(op: Operation) -> Operation:
    if isinstance(op, Jmp):
        return Nop(op.val)
    elif isinstance(op, Nop):
        return Jmp(op.val)
    else:
        return op


def _execute_program_finding_fix(
    program: List[Operation],
    state: PCState,
    executed_instructions: Set[int],
    can_flip_flop=False,
) -> Tuple[bool, PCState]:
    while state.pc < len(program):
        if state.pc in executed_instructions:
            return False, state

        next_instruction = program[state.pc]
        if can_flip_flop and isinstance(next_instruction, (Jmp, Nop)):
            alt_program = program.copy()
            alt_instruction = flip_flop(next_instruction)
            alt_program[state.pc] = alt_instruction
            alt_state = alt_instruction.execute(state)

            alt_final_res, alt_final_state = _execute_program_finding_fix(
                alt_program, alt_state, executed_instructions.copy()
            )
            if alt_final_res:
                return True, alt_final_state

        executed_instructions.add(state.pc)
        state = next_instruction.execute(state)
    return True, state


def puzzle2(input_file: Path):
    state = PCState(0, 0)
    program = _read_program(input_file)
    executed_instructions: Set[int] = set()
    res, state = _execute_program_finding_fix(
        program=program,
        state=state,
        executed_instructions=executed_instructions,
        can_flip_flop=True,
    )
    assert res, "No fix found for the program"
    return state.accumulator


if __name__ == "__main__":
    print("Day 8")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
