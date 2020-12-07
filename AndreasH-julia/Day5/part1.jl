using Printf
function extractBoardingId(boardingPass)
    return parse(Int,replace(replace(replace(replace(boardingPass,'F'=>"0"),'B'=>"1"),'L'=>"0"),'R'=>"1");base=2)
end
data = readlines(open("input.txt","r"))
highestID = 0
for line in data
    id = extractBoardingId(line)
    if id >  highestID
        global highestID = id
    end
end
@printf("Highest boarding pass ID: %d",highestID)
