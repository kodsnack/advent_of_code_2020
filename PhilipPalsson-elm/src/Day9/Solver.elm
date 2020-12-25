module Day9.Solver exposing (solve1, solve2)

import Day9.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import List.Extra


preludeLength =
    25


sumPairExists : Int -> List Int -> Bool
sumPairExists toFind =
    List.Extra.uniquePairs >> List.any (\( a, b ) -> a + b == toFind)


problem1 : Int
problem1 =
    Day9.Input.input
        |> String.split "\n"
        |> List.filterMap String.toInt
        |> List.foldl
            (\value ( earlierValues, res ) ->
                if List.length earlierValues < preludeLength then
                    ( value :: earlierValues, res )

                else if res /= Nothing then
                    ( earlierValues, res )

                else if sumPairExists value (List.take preludeLength earlierValues) then
                    ( value :: earlierValues, res )

                else
                    ( value :: earlierValues, Just value )
            )
            ( [], Nothing )
        |> Tuple.second
        |> Maybe.withDefault 0


solve1 : String
solve1 =
    problem1 |> String.fromInt


findContiguousSet : Int -> Int -> List Int -> Maybe Int
findContiguousSet toFind index =
    List.drop index
        >> List.foldl
            (\val ( numbers, solution ) ->
                if solution /= Nothing then
                    ( [], solution )

                else if List.sum numbers + val == toFind then
                    ( []
                    , Maybe.map2
                        (+)
                        (List.minimum (val :: numbers))
                        (List.maximum (val :: numbers))
                    )

                else
                    ( val :: numbers, Nothing )
            )
            ( [], Nothing )
        >> Tuple.second


solve2 : String
solve2 =
    let
        numbers =
            Day9.Input.input
                |> String.split "\n"
                |> List.filterMap String.toInt

        toFind =
            problem1
    in
    List.range 0 (List.length numbers)
        |> List.foldl
            (\index solution ->
                if solution /= Nothing then
                    solution

                else
                    findContiguousSet toFind index numbers
            )
            Nothing
        |> Maybe.withDefault 0
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
