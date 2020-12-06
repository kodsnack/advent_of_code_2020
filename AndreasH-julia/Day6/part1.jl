using Printf
using Test
@test sum(map((x) -> length(Set(split(replace(x,"\n" => ""),""))) ,split(read("./testinput.txt",String),"\n\n"))) == 11
@printf("Answer: %d",sum(map((x) -> length(Set(split(replace(x,"\n" => ""),""))) ,split(read("./input.txt",String),"\n\n"))))
