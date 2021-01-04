from pathlib import Path
import re
from typing import Callable, List, Union
import operator

IntIsh = Union[str, int]

OPERATIONS = {
    "*": operator.mul,
    "+": operator.add,
}


def tokenize(line: str) -> List[str]:
    return re.findall(r"[0-9\+\*\(\)]", line.strip())


def read_home_work(input_file: Path):
    with open(input_file) as f:
        lines = [tokenize(line) for line in f]
    return lines


def calc(tokens: List[str], evaluator: Callable[[List[IntIsh]], int]):
    parantheses = []
    final_expr = []
    for token in tokens:
        if token == "(":
            parantheses.insert(0, token)
        elif token == ")":
            start_idx = parantheses.index("(")
            simple_expr = parantheses[:start_idx]
            simple_expr.reverse()
            parantheses = parantheses[start_idx + 1 :]
            val = evaluator(simple_expr)
            if parantheses:
                parantheses.insert(0, val)
            else:
                final_expr.append(val)
        elif not parantheses:
            final_expr.append(token)
        else:
            parantheses.insert(0, token)
        # print(final_expr)
    # print(parantheses)
    # print("final_expr:", final_expr)
    return evaluator(final_expr)


def eval_simple(tokens: List[IntIsh]) -> int:
    assert "(" not in tokens and ")" not in tokens
    # print("eval_simple:", tokens)
    total = int(tokens[0])
    op = None
    for token in tokens[1:]:
        if isinstance(token, str) and not token.isnumeric():
            op = OPERATIONS[token]
        else:
            total = op(total, int(token))
    return total


def eval_advanced(tokens: List[IntIsh]) -> int:
    assert "(" not in tokens and ")" not in tokens
    while "+" in tokens:
        idx = tokens.index("+")
        val = int(tokens[idx - 1]) + int(tokens[idx + 1])
        tokens = tokens[: idx - 1] + [val] + tokens[idx + 2 :]
    return eval_simple(tokens)


def puzzle1(input_file: Path):
    tot = 0
    for tokens in read_home_work(input_file):
        val = calc(tokens, evaluator=eval_simple)
        tot += val
    return tot


def puzzle2(input_file: Path):
    tot = 0
    for tokens in read_home_work(input_file):
        val = calc(tokens, evaluator=eval_advanced)
        tot += val
    return tot


if __name__ == "__main__":
    print("Day 18")
    input_file = Path(__file__).parent / "input.txt"
    print("Puzzle 1:", puzzle1(input_file))
    print("Puzzle 2:", puzzle2(input_file))
