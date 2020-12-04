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
            print(mitem)
            found = False
    if found:
        count += 1
    print(count)

if __name__ == "__main__":

    main()