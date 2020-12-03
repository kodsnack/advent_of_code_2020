using Printf

function readfile(filename)
    try
        open(filename,"r") do io
            passwords = []
            inputs = readlines(io)
            for input in inputs
                regex_extraction = match(r"(\d{1,3})-(\d+) (\w): (\w+)",input)
                policy_occurence = parse.(Int,[regex_extraction[1],regex_extraction[2]])
                policy_letter = regex_extraction[3]
                policy_password = regex_extraction[4]

                password = Dict("policy_occurence"=>policy_occurence,"policy_letter"=>policy_letter[1],"password"=>policy_password)
                append!(passwords,[password])
            end

            return passwords
        end;
    catch
        print("Error parsing file")
        return []
    end
end

function validPassword(passwordDict)
    letterCount = length(collect(eachmatch(Regex(string(passwordDict["policy_letter"])),passwordDict["password"])))
    if passwordDict["policy_occurence"][1] <= letterCount <= passwordDict["policy_occurence"][2]
        return true
    end
    return false

end
end


passwords = readfile("input.txt")
validPasswordCount = 0
for password in passwords
    if validPassword(password)
        global validPasswordCount +=1
    end
end

@printf("\n There are %d valid passwords",validPasswordCount)
