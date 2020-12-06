## Start of header boilerplate #################################################

from aocbase import readInput
import re

oneLinePattern = re.compile(r"^.*$")
wsTokenPattern = re.compile(r"\S+")

def lineParse(s, f, fp):
    m = re.findall(fp, s)
    if m==None:
        raise ValueError("No token patterns found")
    return tuple(map(f, m))

def fileParse(inp, lineparser=lineParse,
                   tokenparser=lambda x:x,
                   tokenPattern=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:x, 
                    map(lambda x:lineparser(x, tokenparser, tokenPattern),
                        inp.splitlines())))

## End of header boilerplate ###################################################

def accumulate(itemLines):
    l = list()
    d = dict()
    for line in itemLines:
        if line == ():
            l.append(d)
            d = dict()
        else:
            for item in line:
                a, b = item.split(":")
                d[a] = b
    if len(d) > 0:
        l.append(d)
    return l

def simpleValid(passport):
    return all(item in passport for item in ("byr", "iyr", "eyr",
                                             "hgt", "hcl", "ecl", "pid"))

def part1(pinp):
    return sum(simpleValid(passport) for passport in accumulate(pinp))

def validYear(s, low, high):
    try:
        y = int(s)
    except:
        return False
    return low <= y <= high

def validByr(passport):
    return "byr" in passport and validYear(passport["byr"], 1920, 2002)

def validIyr(passport):
    return "iyr" in passport and validYear(passport["iyr"], 2010, 2020)

def validEyr(passport):
    return "eyr" in passport and validYear(passport["eyr"], 2020, 2030)

def validHgt(passport):
    if "hgt" not in passport:
        return False
    try:
        unit, measure = passport["hgt"][-2:], int(passport["hgt"][:-2])
    except:
        return False
    return ((unit == "cm" and 150 <= measure <= 193) or
            (unit == "in" and 59 <= measure <= 76))

def validHcl(passport):
    if "hcl" not in passport:
        return False
    hcl = passport["hcl"]
    return (len(hcl) == 7 and hcl[0]=="#" and 
            all(c in "0123456789abcdef" for c in hcl[1:]))

def validEcl(passport):
    return ("ecl" in passport and
            passport["ecl"] in ("amb","blu","brn","gry","grn","hzl","oth"))

def validPid(passport):
    if "pid" not in passport:
        return False
    pid = passport["pid"]
    return (len(pid) == 9 and all(c in "0123456789" for c in pid))

def complexValid(passport):
    return (validByr(passport) and validIyr(passport) and validEyr(passport) and
            validHgt(passport) and validHcl(passport) and validEcl(passport) and
            validPid(passport))

def part2(pinp):
    return sum(complexValid(passport) for passport in accumulate(pinp))

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, tokenPattern=wsTokenPattern)

    print("Input is '" + str(parseInp[:10])[:160] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>160 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
