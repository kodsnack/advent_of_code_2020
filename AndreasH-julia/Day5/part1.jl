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
data = readlines(open("input.txt","r"))
highestID = 0
for line in data
    row,col = extractPosition(line)
    id = row * 8 + col
    if id >  highestID
        global highestID = id
    end
end
@printf("Highest boarding pass ID: %d",highestID)
