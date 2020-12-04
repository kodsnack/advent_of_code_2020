import re
def solve(lines):
    passports = []
    passport = {}
    for i in range(len(lines)):
        if ord(lines[i][0]) == 10:
            passports.append(passport)
            passport = {}
        else:
            fields = lines[i].split()
            for f in range(len(fields)):
                (k,v) = fields[f].split(':')
                passport[k] = v
    passports.append(passport)

    valid = 0
    for i in range(len(passports)):
        p = passports[i]
        if ('byr' in p and
            'iyr' in p and
            'eyr' in p and
            'hgt' in p and
            'hcl' in p and
            'ecl' in p and
            'pid' in p
        ):
            valid += 1
    return valid
if __name__ == '__main__':
    lines = []
    with open('4.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
