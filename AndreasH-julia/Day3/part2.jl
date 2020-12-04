using Printf

function readfile(filename)
    try
        open(filename,"r") do io
            inputs = readlines(io)
            return inputs
        end;
    catch
        print("Error parsing file")
        return []
    end
end

function takeStep(map,position,stepSize,treeCount=0)
    try
        forrestLine= map[position[2]]
        xPos = mod(position[1],length(map[1])) == 0 ? length(map[1]) : mod(position[1],length(map[1]))
        value = forrestLine[xPos]
        if string(value) == "#"
            takeStep(map,position + stepSize,stepSize,treeCount + 1)
        else
            takeStep(map,position + stepSize,stepSize,treeCount)
        end
    catch
        return treeCount
    end
end


forrest = readfile("input.txt")
slopes=[[1,1],[3,1],[5,1],[7,1],[1,2]]
slopeTreeEncounters = []
finalProduct = 1
for slope in slopes
    append!(slopeTreeEncounters, takeStep(forrest,[1,1],slope))
    @printf("\nNumber of trees encountered for slope (%d,%d): %d",slope[1],slope[2],last(slopeTreeEncounters))
    global finalProduct *= last(slopeTreeEncounters)
end
@printf("\nFinal product of tree encounters: %d",finalProduct)
