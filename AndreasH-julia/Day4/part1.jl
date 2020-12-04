using Printf
data = split(read("./input.txt",String),"\n\n")
regexCommand = r"(\w{3}:)"
validPassports = 0
for line in data
    fields = map((x) -> x.match ,collect(eachmatch(regexCommand,line)))
    if length(fields) == 8 ||(length(fields) == 7 && !in("cid:",fields))
        global validPassports += 1
    end
end
@printf("Amount of valid passports: %d",validPassports)
