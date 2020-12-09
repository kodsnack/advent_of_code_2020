using Printf
using Test
using Combinatorics
testinput = parse.(Int,readlines("testinput.txt"))
realData = parse.(Int,readlines("input.txt"))
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
@testset "Test Data" begin
        @test testinput[findInvalidNumber(testinput,5)] == 127
end
@printf("\nAnswer: %d",realData[findInvalidNumber(realData,25)])
