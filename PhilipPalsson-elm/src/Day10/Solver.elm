module Day10.Solver exposing (solve1, solve2)

import Day10.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra


solve1 : String
solve1 =
    let
        numbers =
            Day10.Input.input
                |> String.split "\n"
                |> List.filterMap String.toInt
                |> List.sort

        differences =
            List.map2
                (\a b -> b - a)
                (0 :: numbers)
                (numbers ++ [ List.maximum numbers |> Maybe.map ((+) 3) |> Maybe.withDefault 0 ])

        numberOf1 =
            List.filter ((==) 1) differences |> List.length

        numberOf3 =
            List.filter ((==) 3) differences |> List.length
    in
    numberOf1 * numberOf3 |> String.fromInt


helper2 : Int -> Int -> List Int -> Int
helper2 end current rest =
    if current == end then
        1

    else
        rest
            |> List.filter (\a -> a > current && abs (current - a) <= 3)
            |> List.foldl
                (\adapter count ->
                    count + helper2 end adapter rest
                )
                0


solve2 : String
solve2 =
    let
        numbers =
            Day10.Input.input
                |> String.split "\n"
                |> List.filterMap String.toInt
                |> List.sort
    in
    (0 :: numbers)
        |> List.Extra.groupWhile (\a b -> abs (a - b) < 3)
        |> List.map
            (\( start, rest ) ->
                helper2 (List.Extra.last rest |> Maybe.withDefault start) start rest
            )
        |> List.product
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
