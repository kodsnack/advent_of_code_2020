using Printf
using Test
function countAnswers(input)
    groupsCharacters = map((x) -> Set(split(replace(x,"\n" => ""),"")),input)
    counter = 0
    for (index,group) in enumerate(groupsCharacters)
        for character in group
            if sum([1 for i = eachmatch(Regex(character),input[index])]) > sum([1 for i = eachmatch(Regex("\\n"),chomp(input[index]))])
                counter+=1
            end
        end
    end
    return counter
end
@test countAnswers(split(read("./testinput.txt",String),"\n\n")) == 6
@printf("Answer %d",countAnswers(split(read("./input.txt",String),"\n\n")))
