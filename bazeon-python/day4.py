from collections import namedtuple
import string


def get_input():
    with open("input_day4.txt", 'r') as input_file:
        lines = input_file.readlines()
    return lines


p = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']


def check_passport(passport):
    for passp in passport:
        if not passp:
            return False
    return True


def ishex(s):
    for c in s:
        if c not in string.hexdigits:
            return False

    return True


def validate_passport(passport):
    if not 1920 <= int(passport[0]) <= 2002:
        return False

    if not 2010 <= int(passport[1]) <= 2020:
        return False

    if not 2020 <= int(passport[2]) <= 2030:
        return False

    if not ((passport[3][-2:] == 'in' and 59 <= int(passport[3][:-2]) <= 76) or (passport[3][-2:] == 'cm' and 150 <= int(passport[3][:-2]) <= 193)):
        return False

    if not (len(passport[4]) == 7 and passport[4][0] == '#' and ishex(passport[4][1:0])):
        return False

    if not passport[5] in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']:
        return False

    if not (len(passport[6]) == 9 and passport[6].isnumeric()):
        return False

    return True


def scan_passports():
    passports = []
    lines = get_input()

    npass = [False for i in range(7)]

    for line in lines:

        if line == '\n':
            passports.append(npass)
            npass = [False for i in range(7)]

        else:
            entries = line.split()
            for entry in entries:

                field, rest = entry.split(':')

                try:
                    npass[p.index(field)] = rest
                except ValueError:
                    pass

    passports.append(npass)

    return passports

def solve_a():

    passports = scan_passports()

    c = 0
    for pa in passports:
        if check_passport(pa):
            c += 1

    return c


def solve_b():

    passports = scan_passports()

    c = 0
    for pa in passports:
        if check_passport(pa):
            if validate_passport(pa):
                c += 1

    return c


print(str(solve_a()))
print(str(solve_b()))
