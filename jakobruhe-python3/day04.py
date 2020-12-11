#!/usr/bin/env python3

# By Jakob Ruhe 2020-12-04
# 06:22 p1 solved
# 07:03 p2 solved

import unittest
import re
from collections import defaultdict
from collections import namedtuple

def parse_input(input):
    passports = []
    lines = input.strip().split("\n");
    p = {}
    for line in lines:
        if not line and p:
            passports.append(p)
            p = {}
            continue
        key_value_pairs = line.split(" ")
        for kvp in key_value_pairs:
            key, value = kvp.split(":")
            p[key] = value
    if p:
        passports.append(p)
    return passports

def is_valid_passport1(p):
    required_fields = (
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid")
    for f in required_fields:
        if f not in p:
            return False
    return True

def solve1(entries):
    return sum(is_valid_passport1(e) for e in entries)

def byr_valid(value):
    m = re.match(r"^(\d\d\d\d)$", value)
    if not m:
        return False
    v = int(m.group(1))
    return v >= 1920 and v <= 2002

def iyr_valid(value):
    m = re.match(r"^(\d\d\d\d)$", value)
    if not m:
        return False
    v = int(m.group(1))
    return v >= 2010 and v <= 2020

def eyr_valid(value):
    m = re.match(r"^(\d\d\d\d)$", value)
    if not m:
        return False
    v = int(m.group(1))
    return v >= 2020 and v <= 2030

def hgt_valid(value):
    m = re.match(r"^(\d+)(cm|in)$", value)
    if not m:
        return False
    v, u = int(m.group(1)), m.group(2)
    if u == "cm":
        return v >= 150 and v <= 193
    elif u == "in":
        return v >= 59 and v <= 76
    else:
        return False

def hcl_valid(value):
    m = re.match(r"^#([0-9a-f]{6})$", value)
    return bool(m)

def ecl_valid(value):
    m = re.match(r"^(amb|blu|brn|gry|grn|hzl|oth)$", value)
    return bool(m)

def pid_valid(value):
    m = re.match(r"^(\d){9}$", value)
    return bool(m)

def is_valid_key_value(key, value):
    validators = {
        "byr": byr_valid,
        "iyr": iyr_valid,
        "eyr": eyr_valid,
        "hgt": hgt_valid,
        "hcl": hcl_valid,
        "ecl": ecl_valid,
        "pid": pid_valid,
        "cid": lambda v: True,
    }
    return validators[key](value)

def is_valid_passport2(p):
    if not is_valid_passport1(p):
        return False
    for key, value in p.items():
        if not is_valid_key_value(key, value):
            return False
    return True

def solve2(entries):
    return sum(is_valid_passport2(e) for e in entries)

# Execute tests with:
# python3 -m unittest dayX
class TestThis(unittest.TestCase):
    input = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
"""
    def test1(self):
        self.assertEqual(solve1(parse_input(self.input)), 2)
    def test2(self):
        self.assertEqual(is_valid_key_value("byr", "2002"), True)
        self.assertEqual(is_valid_key_value("byr", "2003"), False)
        self.assertEqual(is_valid_key_value("hgt", "60in"), True)
        self.assertEqual(is_valid_key_value("hgt", "190cm"), True)
        self.assertEqual(is_valid_key_value("hgt", "190in"), False)
        self.assertEqual(is_valid_key_value("hgt", "190"), False)
        self.assertEqual(is_valid_key_value("hcl", "#123abc"), True)
        self.assertEqual(is_valid_key_value("hcl", "#123abz"), False)
        self.assertEqual(is_valid_key_value("hcl", "123abc"), False)
        self.assertEqual(is_valid_key_value("ecl", "brn"), True)
        self.assertEqual(is_valid_key_value("ecl", "wat"), False)
        self.assertEqual(is_valid_key_value("pid", "000000001"), True)
        self.assertEqual(is_valid_key_value("pid", "0123456789"), False)

        invalid = """eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
"""
        self.assertEqual(solve2(parse_input(invalid)), 0)

        valid = """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
        """
        self.assertEqual(solve2(parse_input(valid)), 4)

if __name__ == "__main__":
    with open("input/day04.txt") as f:
        input = f.read()
    entries = parse_input(input)
    print(f"{solve1(entries)}")
    print(f"{solve2(entries)}")
