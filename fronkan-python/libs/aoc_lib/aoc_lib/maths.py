from functools import reduce
from typing import Iterable
import operator


def prod(nums: Iterable[int]) -> int:
    return reduce(operator.mul, nums)
