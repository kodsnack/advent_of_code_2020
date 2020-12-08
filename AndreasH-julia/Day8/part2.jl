using Printf
using Test
testData = readlines("./testinput.txt")
realData = readlines("./input.txt")
rowHistory = []
accValue = 0
function nextLine(data,nextIndex)
    if nextIndex > length(data)
        return 1
    elseif nextIndex in rowHistory
        return 0
    end
    push!(rowHistory,nextIndex)
    command = data[nextIndex][1:3]
    value = data[nextIndex][4:end]
    previousIndex = nextIndex
    if command == "nop"
        nextIndex += 1
    elseif command == "acc"
        global accValue += parse(Int,value)
        nextIndex += 1
    elseif command == "jmp"
        nextIndex += parse(Int,value)
    end
    nextLine(data,nextIndex)
end

# Really crappy solution but I got stuck so brute forced.
function solve(dataPath)
    dataSet = readlines(dataPath)
    for (index,line) in enumerate(dataSet)
        global accValue = 0
        global rowHistory = []
        data = readlines(dataPath)
        command = line[1:3]
        value = line[4:end]
        if command == "nop" && parse(Int,value) != 0
            data[index] = "jmp"*line[4:end]
        elseif command =="jmp"
            data[index] = "nop"*line[4:end]
        end
        if nextLine(data,1) == 1
            break
        end
    end
end

@testset "Test data" begin
    solve("testinput.txt")
    @test accValue == 8
end

solve("input.txt")
@printf("Answer: %d",accValue)
