module Day6.Solver exposing (solve1, solve2)

import Day6.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Set


solve1 : String
solve1 =
    Day6.Input.input
        |> String.split "\n\n"
        |> List.map
            (String.replace "\n" ""
                >> String.split ""
                >> Set.fromList
                >> Set.size
            )
        |> List.sum
        |> String.fromInt


solve2 : String
solve2 =
    Day6.Input.input
        |> String.split "\n\n"
        |> List.map
            (String.split "\n"
                >> List.map (String.split "" >> Set.fromList)
                >> List.foldl Set.intersect
                    ("abcdefghijklmnopqrstuvwxyz"
                        |> String.split ""
                        |> Set.fromList
                    )
                >> Set.size
            )
        |> List.sum
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
