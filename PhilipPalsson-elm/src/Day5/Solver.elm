module Day5.Solver exposing (..)

import Day5.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra


seatStringToInt : String -> Int
seatStringToInt =
    String.reverse
        >> String.split ""
        >> List.indexedMap
            (\index s ->
                if s == "F" || s == "L" then
                    0

                else
                    2 ^ index
            )
        >> List.sum


solve1 : String
solve1 =
    Day5.Input.input
        |> String.split "\n"
        |> List.map seatStringToInt
        |> List.maximum
        |> Maybe.withDefault 0
        |> String.fromInt


solve2 : String
solve2 =
    let
        seatIds =
            Day5.Input.input
                |> String.split "\n"
                |> List.map seatStringToInt
                |> List.sort

        offset =
            List.head seatIds |> Maybe.withDefault 0
    in
    List.indexedMap Tuple.pair seatIds
        |> List.Extra.findIndex (\( index, seatId ) -> seatId /= index + offset)
        |> Maybe.map ((+) offset)
        |> Maybe.withDefault 0
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
