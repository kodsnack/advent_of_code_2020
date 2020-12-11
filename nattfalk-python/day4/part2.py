import re

def validate(code, val):
    if code == "byr":
        return 1920 <= int(val) <= 2002
    elif code == "iyr":
        return 2010 <= int(val) <= 2020
    elif code == "eyr":
        return 2020 <= int(val) <= 2030
    elif code == "hgt":
        if val[-2:] == "cm":
            return 150 <= int(val[:-2]) <= 193
        elif val[-2:] == "in":
            return 59 <= int(val[:-2]) <= 76
        else:
            return False
    elif code == "hcl":
        return bool(re.match('^#[0-9a-f]{6}$', val))
    elif code == "ecl":
        return val in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']
    elif code == "pid":
        return bool(re.match('^[0-9]{9}$', val))
    elif code == "cid":
        return True
    else:
        return False

valid_codes = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] # optional cid

valid_parts = 0
valid_pws = 0
with open('input.txt', 'r') as f:
    for line in f:
        if line.strip():
            parts = line.strip().split(' ')
            for part in parts:
                (code, val) = part.split(':')
                if code in valid_codes and validate(code, val):
                    valid_parts += 1
        else:
            if valid_parts == len(valid_codes):
                valid_pws += 1
            valid_parts = 0
f.close()

if valid_parts == len(valid_codes):
    valid_pws += 1

print(f'Found {valid_pws} valid pw''s')
