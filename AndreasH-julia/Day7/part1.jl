using Printf
using Test
testData = readlines("./testinput1.txt")
realData = readlines("./input.txt")
availableBags = Set{String}()
function findavailable(bagType,isTest = true)
    if isTest
        data = testData
    else
        data = realData
    end
    regexCommand = Regex("(.+) bags.+contain.+"*bagType)
    for line in data
        regexMatch = match(regexCommand,line)
        if regexMatch  != nothing
            push!(availableBags, string(regexMatch[1]))
            findavailable(regexMatch[1],isTest)
        end
    end
end
function solve(isTest = true)
    empty!(availableBags)
    findavailable("shiny gold",isTest)
    return length(availableBags)
end
@testset "Test data control" begin
    @test solve() == 4
end
@printf("Amount of options: %d",solve(false))
