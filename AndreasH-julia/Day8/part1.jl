using Printf
using Test
testData = readlines("./testinput.txt")
realData = readlines("./input.txt")
rowHistory = []
accValue = 0
function nextLine(data,nextIndex)
    if length(data) >= nextIndex
        if nextIndex in rowHistory
            return
        end
        push!(rowHistory,nextIndex)
        command = data[nextIndex][1:3]
        value = data[nextIndex][4:end]
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
end
@testset "Test Data control" begin
    global accValue = 0
    global rowHistory = []
    nextLine(testData,1)
    @test accValue == 5
    global accValue = 0
    global rowHistory = []
end
nextLine(realData,1)
@printf("Answer: %d",accValue)
