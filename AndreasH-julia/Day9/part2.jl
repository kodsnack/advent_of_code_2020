using Printf
using Test
using Combinatorics
testinput = parse.(Int,readlines("testinput.txt"))
realData = parse.(Int64,readlines("input.txt"))
function validNumber(data,index,lookBack)
        comb = combinations(data[index-lookBack:index-1],2)
        for combination in comb
                if sum(combination) == data[index]
                        return true
                end
        end
        return false
end
function findInvalidNumber(data,lookback)
        for index in range(1,stop=length(data))
                if index > lookback
                        if !validNumber(data,index,lookback)
                                return index
                        end
                end
        end
end
function findMessage(data,lookback)
        invalidNumberIndex = findInvalidNumber(data,lookback)
        println(invalidNumberIndex)
        for start in range(1,stop=invalidNumberIndex-1)
                for size in range(1,stop=invalidNumberIndex-start)
                        if sum(data[start:start+size]) == data[invalidNumberIndex]
                                numbers = sort(data[start:start+size])
                                return numbers[1] + numbers[end]
                        end
                end
        end
end
@testset "Test Data" begin
        @test findMessage(testinput,5) == 62
end
@printf("\nAnswer: %d",findMessage(realData,25))
