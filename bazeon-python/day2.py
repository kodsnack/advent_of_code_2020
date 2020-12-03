def get_input():
    with open("input_day2.txt", 'r') as input_file:
        lines = input_file.readlines()
    return lines


def is_password_correct_a(line):
    limits, letter, passw = line.split()
    letter = letter[0]
    lower, upper = limits.split("-")

    return int(lower) <= passw.count(letter) <= int(upper)


def how_many_valid_a():
    lines = get_input()
    valid = 0
    for line in lines:
        if is_password_correct_a(line):
            valid += 1
    return valid


print(how_many_valid_a())


def is_password_correct_b(line):
    limits, letter, passw = line.split()
    letter = letter[0]
    first, second = limits.split("-")
    return (passw[int(first) - 1] == letter) ^ (passw[int(second) - 1] == letter)


def how_many_valid_b():
    lines = get_input()
    valid = 0
    for line in lines:
        if is_password_correct_b(line):
            valid += 1
    return valid


print(how_many_valid_b())
