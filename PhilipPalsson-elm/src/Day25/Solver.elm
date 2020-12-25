module Day25.Solver exposing (solve)

import Day25.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


findLoopSize : Int -> Int -> Int -> Int -> Int
findLoopSize subject value toFind loopCount =
    let
        nextValue =
            subjectCardTransformationStep subject value
    in
    if nextValue == toFind then
        loopCount

    else
        findLoopSize subject nextValue toFind (loopCount + 1)


transformSubject subject loopSize =
    List.foldl (\_ acc -> subjectCardTransformationStep subject acc) 1 (List.range 1 loopSize)


subjectCardTransformationStep : Int -> Int -> Int
subjectCardTransformationStep subject value =
    remainderBy 20201227 (subject * value)


solve : Int
solve =
    let
        cardLoopSize =
            findLoopSize 7 1 Day25.Input.cardPublic 1
    in
    transformSubject Day25.Input.doorPublic cardLoopSize


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution: " ++ (solve |> String.fromInt)) ]
        ]
