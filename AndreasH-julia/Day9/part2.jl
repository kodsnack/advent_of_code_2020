using Printf
using Test
using Combinatorics
include("part1.jl")
testinput = parse.(Int,readlines("testinput.txt"))
realData = parse.(Int64,readlines("input.txt"))
function findMessage(data,lookback)
        invalidNumberIndex = findInvalidNumber(data,lookback)
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
