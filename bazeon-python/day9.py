preamble_len = 25


def initialize():
    numbers = []
    with open("input_day9.txt", 'r') as input_file:
        lines = input_file.readlines()
        for line in lines:
            numbers.append(int(line))
    return numbers


def solve_a():
    numbers = initialize()
    previous = numbers[:preamble_len]
    numbers = numbers[preamble_len:]

    for n in numbers:
        if not factors_exists(previous, n):
            return n
        previous.append(n)
        previous.pop(0)

    return None


def factors_exists(numbers, numb):
    for n in numbers:
        if (numb - n) in numbers and n != (numb - n):
            return True
    return False


def solve_b():
    invalid = solve_a()
    numbers = initialize()
    ahead = numbers.copy()
    summed = []
    total = 0

    for n in numbers:
        total = n
        summed.append(n)
        ahead.pop(0)

        i = 0
        while total < invalid:
            summed.append(ahead[i])
            total += ahead[i]
            i += 1

        if total == invalid:
            return max(summed) + min(summed)

        total = 0
        summed.clear()


print(solve_a())

print(solve_b())
