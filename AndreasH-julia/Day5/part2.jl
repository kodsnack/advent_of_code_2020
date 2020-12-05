using Printf
function extractPosition(boardingPass)
    rowBin = ""
    colBin = ""
    for character in boardingPass[1:7]
        rowBin *= character == 'F' ? "0" : "1"
    end
    for character in boardingPass[8:10]
        colBin *= character == 'L' ? "0" : "1"
    end
    return parse(Int,rowBin;base=2),parse(Int,colBin;base=2)
end
rowIDs = []
data = readlines(open("input.txt","r"))
# Create list of boarding pass IDs
for line in data
    row,col = extractPosition(line)
    id = row * 8 + col
    push!(rowIDs,id)
end
# If we find where the difference is 2, then we found our boarding pass
for (index,line) in enumerate(sort!(rowIDs))
    if (rowIDs[index+1] - line) == 2
        @printf("Your boarding pass ID is : %d",line+1)
        break
    end
end
