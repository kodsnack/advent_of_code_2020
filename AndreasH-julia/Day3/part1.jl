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

function takeStep(map,position)
    try
        forrestLine= map[position[2]]
        xPos = mod(position[1],length(map[1])) == 0 ? length(map[1]) : mod(position[1],length(map[1]))
        value = forrestLine[xPos]
        if string(value) == "#"
            global treeCount += 1
        end
        takeStep(map,position + stepSize)
    catch 
        return
    end
end


forrest = readfile("input.txt")
stepSize = [3,1]
treeCount = 0
takeStep(forrest,[1,1])
@printf("\nNumber of trees encountered: %d",treeCount)
