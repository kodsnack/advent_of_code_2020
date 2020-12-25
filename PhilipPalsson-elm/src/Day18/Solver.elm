module Day18.Solver exposing (solve1, solve2)

import Day18.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Regex exposing (Match)
import String.Extra


toInt =
    String.toInt >> Maybe.withDefault 0


calculateFromLeftToRight : String -> Int
calculateFromLeftToRight string =
    case String.trim string |> String.split " " of
        firstDigit :: operator :: secondDigit :: rest ->
            let
                a =
                    toInt firstDigit

                b =
                    toInt secondDigit

                result =
                    if operator == "+" then
                        a + b

                    else
                        a * b
            in
            calculateFromLeftToRight ((result |> String.fromInt) ++ " " ++ String.join " " rest)

        digit :: [] ->
            toInt digit

        _ ->
            0


doAllParenthesisedCalculations : Bool -> String -> String
doAllParenthesisedCalculations shouldDoAdditionFirst string =
    let
        matches =
            Regex.findAtMost
                1
                (Regex.fromString "\\([^\\(\\)]*\\)" |> Maybe.withDefault Regex.never)
                string

        helper match s =
            String.Extra.replaceSlice
                (match.match
                    |> String.dropLeft 1
                    |> String.dropRight 1
                    |> calculateFromLeftToRight
                    |> String.fromInt
                )
                match.index
                (match.index + String.length match.match)
                s
    in
    if List.isEmpty matches then
        string

    else
        List.foldl
            helper
            string
            matches
            |> (\s ->
                    if shouldDoAdditionFirst then
                        doAllAddition s

                    else
                        s
               )
            |> doAllParenthesisedCalculations shouldDoAdditionFirst


solve1 : Int
solve1 =
    Day18.Input.input
        |> String.lines
        |> List.map
            (doAllParenthesisedCalculations False
                >> calculateFromLeftToRight
            )
        |> List.sum


doAllAddition : String -> String
doAllAddition string =
    let
        result =
            Regex.replace
                (Regex.fromString "\\d+ [+] \\d+" |> Maybe.withDefault Regex.never)
                (\match ->
                    calculateFromLeftToRight match.match |> String.fromInt
                )
                string
    in
    if result /= string then
        doAllAddition result

    else
        result


solve2 : Int
solve2 =
    Day18.Input.input
        |> String.lines
        |> List.map
            (doAllAddition
                >> doAllParenthesisedCalculations True
                >> calculateFromLeftToRight
            )
        |> List.sum


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ (solve1 |> String.fromInt)) ]
        , div [] [ text ("Solution part 2: " ++ (solve2 |> String.fromInt)) ]
        ]
