using Printf

function readfile(filename)
    try
        open(filename,"r") do io
            inputs = parse.(Int,readlines(io))
            return inputs
        end;
    catch
        print("Error parsing file")
        return []
    end
end;

inputs = readfile("input.txt")
for (index_1,input_1) in enumerate(inputs)
    for (index_2,input_2) in enumerate(inputs)
        if index_1 != index_2
            if input_1 + input_2 == 2020
                @printf("Values: %d and %d sum up to 2020. The product of these are: %d",input_1,input_2,input_1 * input_2)
            end

        end
    end
end
