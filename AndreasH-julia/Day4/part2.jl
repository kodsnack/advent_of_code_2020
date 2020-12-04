using Printf
data = split(read("./input.txt",String),"\n\n")
regexCommand = r"(\w{3}:)"
validPassports = 0
regexConditions = [r"byr:(19[2-8][0-9]|199[0-9]|200[0-2])",r"iyr:(201[0-9]|2020)",r"eyr:(202[0-9]|2030)",r"hgt:(59in|6[0-9]in|7[0-6]in|1[5-8][0-9]cm|19[0-3]cm)",r"hcl:#([0-9a-f]{6})",r"ecl:(amb|blu|brn|gry|grn|hzl|oth)",r"pid:\b([0-9]{9})\b"]
for line in data
    fields = map((x) -> x.match ,collect(eachmatch(regexCommand,line)))
    if length(fields) == 8 || (length(fields) == 7 && !in("cid:",fields))
        for condition in regexConditions
            if match(condition,line) === nothing
                @goto next_line
            end
        end
        global validPassports +=1
    end
    @label next_line
end
@printf("Amount of valid passports: %d",validPassports)
