def get_input():
    with open("input_day2.txt", 'r') as input_file:
        lines = input_file.readlines()
    return lines


def is_password_correct_a(line):
    policy, passw = line.split(":")
    limits, letter = policy.split()
    lower, upper = limits.split("-")
    return check_password_a(passw, letter, int(upper), int(lower))


def check_password_a(passw, letter, upper, lower):
    times = passw.count(letter)
    return times >= lower and times <= upper


def how_many_valid_a():
    lines = get_input()
    valid = 0
    for line in lines:
        if is_password_correct_a(line):
            valid += 1
    return valid


print(how_many_valid_a())


def is_password_correct_b(line):
    policy, passw = line.split(":")
    limits, letter = policy.split()
    first, second = limits.split("-")
    return check_password_b(passw.strip(), letter, int(first), int(second))


def check_password_b(passw, letter, first, second):
    c = 0
    if passw[first - 1] == letter:
        c += 1
    if passw[second - 1] == letter:
        c += 1
    return c == 1


def how_many_valid_b():
    lines = get_input()
    valid = 0
    for line in lines:
        if is_password_correct_b(line):
            valid += 1
    return valid


print(how_many_valid_b())
