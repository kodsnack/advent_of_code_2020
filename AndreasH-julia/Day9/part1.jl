using Printf
using Test
using Combinatorics
testinput = parse.(Int,readlines("testinput.txt"))
realData = parse.(Int,readlines("input.txt"))
function validNumber(data,index,lookBack)
        comb = collect(combinations(data[index-lookBack:index-1],2))
        filter!((x) -> sum(x) == data[index],comb)
        return length(comb) > 0
end
function findInvalidNumber(data,lookback)
        for index in range(lookback+1,stop=length(data))
                if !validNumber(data,index,lookback)
                        return index
                end
        end
end
@testset "Test Data" begin
        @test testinput[findInvalidNumber(testinput,5)] == 127
end
@printf("\nAnswer: %d",realData[findInvalidNumber(realData,25)])
