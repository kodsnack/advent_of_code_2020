import re
def main():
    matrix = ['byr', 'iyr', 'eyr', 'hgt','hcl', 'ecl', 'pid']
    items = []
    items2 = []
    passport = []
    count = 0
    found = True
    file = open("input.txt", "r")
    for row in file:
        row = row.strip()
        if row != "":
            items = row.split(' ')
            for item in items:
                if item.split(':')[0] == 'byr':
                    if int(item.split(':')[1]) >= 1920 and int(item.split(':')[1]) <= 2002:
                        passport.append(item.split(':')[0])
                if item.split(':')[0] == 'iyr':
                    if int(item.split(':')[1]) >= 2010 and int(item.split(':')[1]) <= 2020:
                        passport.append(item.split(':')[0])   
                if item.split(':')[0] == 'eyr':
                    if int(item.split(':')[1]) >= 2020 and int(item.split(':')[1]) <= 2030:
                        passport.append(item.split(':')[0]) 
                if item.split(':')[0] == 'hgt':
                    if item.split(':')[1][-2:] == 'cm':
                        if int(item.split(':')[1][:-2]) >= 150 and int(item.split(':')[1][:-2]) <= 193:
                            passport.append(item.split(':')[0])
                    elif item.split(':')[1][-2:] == 'in':
                        if int(item.split(':')[1][:-2]) >= 59 and int(item.split(':')[1][:-2]) <= 76:
                           passport.append(item.split(':')[0])
                if item.split(':')[0] == 'hcl':
                    if re.search("^#[a-zA-Z0-9]{6}$", item.split(':')[1]):
                        passport.append(item.split(':')[0])
                if item.split(':')[0] == 'ecl':
                    if (item.split(':')[1]) in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']:
                        passport.append(item.split(':')[0])
                if item.split(':')[0] == 'pid':
                    if re.search("^[0-9]{9}$", item.split(':')[1]):
                        passport.append(item.split(':')[0])
        else:
            if passport != []:
                for mitem in matrix:
                    if mitem not in passport:
                        found = False
                if found:
                    count += 1
                    found = True
                found = True
            passport = []
    found = True
    for mitem in matrix:
        if mitem not in passport:
            found = False
    if found:
        count += 1
    print(count)

if __name__ == "__main__":

    main()