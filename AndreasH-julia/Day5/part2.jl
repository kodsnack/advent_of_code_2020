using Printf
function extractBoardingId(boardingPass)
    return parse(Int,replace(replace(replace(replace(boardingPass,'F'=>"0"),'B'=>"1"),'L'=>"0"),'R'=>"1");base=2)
end
rowIDs = []
data = readlines(open("input.txt","r"))
for line in data
    id = extractBoardingId(line)
    push!(rowIDs,id)
end
# If we find where the difference is 2, then we found our boarding pass
for (index,line) in enumerate(sort!(rowIDs))
    if (rowIDs[index+1] - line) == 2
        @printf("Your boarding pass ID is : %d",line+1)
        break
    end
end
