
def initialize():
    numbers = []
    with open("input_day10.txt", 'r') as input_file:
        lines = input_file.readlines()
        for line in lines:
            numbers.append(int(line))
    return numbers


def solve_a():
    adapters = initialize()
    adapters.sort()

    jolts = 0
    one_diff = 0
    three_diff = 0
    for a in adapters:
        if a - jolts == 3:
            three_diff += 1
        elif a - jolts == 1:
            one_diff += 1

        jolts = a

    return one_diff * (three_diff + 1)


def solve_b():

    adapters = initialize()
    adapters.append(0)
    adapters.append(max(adapters) + 3)
    adapters.sort()

    size = len(adapters)
    print(size)
    checked = {}

    def arrange(i):
        if i == size - 1:
            return 1

        if i in checked:
            return checked[i]

        arrangements = arrange(i + 1)

        if i < size - 2 and adapters[i + 2] <= adapters[i] + 3:
            arrangements += arrange(i + 2)
        if i < size - 3 and adapters[i + 3] <= adapters[i] + 3:
            arrangements += arrange(i + 3)

        checked[i] = arrangements

        return arrangements

    return arrange(0)


print(solve_a())

print(solve_b())
