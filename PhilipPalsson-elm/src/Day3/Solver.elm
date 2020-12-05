module Day3.Solver exposing (solve1, solve2)

import Day3.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


helper : Int -> (Int -> Maybe Int) -> List String -> Int
helper rightStepLength indexMapper =
    List.indexedMap
        (\rowIndex s ->
            case indexMapper rowIndex of
                Just mappedRowIndex ->
                    let
                        index =
                            remainderBy (String.length s) (mappedRowIndex * rightStepLength)
                    in
                    String.slice index (index + 1) s == "#"

                Nothing ->
                    False
        )
        >> List.filter identity
        >> List.length


solve1 : String
solve1 =
    Day3.Input.input
        |> String.split "\n"
        |> helper 3 Just
        |> String.fromInt


solve2 : String
solve2 =
    let
        rows =
            Day3.Input.input
                |> String.split "\n"

        onlyEven n =
            if remainderBy 2 n == 0 then
                Just (n // 2)

            else
                Nothing
    in
    (helper 1 Just rows
        * helper 3 Just rows
        * helper 5 Just rows
        * helper 7 Just rows
        * helper 1 onlyEven rows
    )
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
