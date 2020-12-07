using Printf
using Test
testData1 = readlines("./testinput1.txt")
testData2 = readlines("./testinput2.txt")
realData = readlines("./input.txt")
function countBags(bagType,isTest = true,testFile = 1)
    bagAmount = 1
    if isTest
        if testFile == 1
            data = testData1
        else
            data = testData2
        end
    else
        data = realData
    end
    regexCommand = Regex(bagType*".+contain")
    regexSubBags = r"[1-9] ([\w\s]+) bag"
    regexSubBagsAmount = r"([1-9]) \w+ \w+"
    for line in data
        if match(regexCommand,line)  != nothing
            if match(regexSubBags,line) != nothing
                subBags = map((x) -> x[1], collect(eachmatch(regexSubBags,line)))
                amounts = map((x) -> parse(Int64,x[1]),collect(eachmatch(regexSubBagsAmount,line)))
                for (type,amount) in zip(subBags,amounts)
                    bagAmount += amount * countBags(type,isTest,testFile)
                end
            end
        end
    end
    return bagAmount
end
@testset "Test data control" begin
    @test countBags("shiny gold",true,1)-1 == 32
    @test countBags("shiny gold",true,2)-1 == 126
end
@printf("Amount of bags: %d",countBags("shiny gold",false)-1)
