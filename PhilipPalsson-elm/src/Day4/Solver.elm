module Day4.Solver exposing (solve1, solve2)

import Day4.Input
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Regex


solve1 : String
solve1 =
    Day4.Input.input
        |> String.split "\n\n"
        |> List.map
            (String.replace "\n" " "
                >> String.split " "
                >> List.filter (String.startsWith "cid:" >> not)
            )
        |> List.filter (List.length >> (==) 7)
        |> List.length
        |> String.fromInt


listToTuple lst =
    case lst of
        a :: b :: [] ->
            ( a, b )

        _ ->
            ( "", "" )


validPassword fields =
    let
        byr =
            Dict.get "byr" fields
                |> Maybe.andThen String.toInt
                |> Maybe.map (\v -> v >= 1920 && v <= 2002)
                |> Maybe.withDefault False

        iyr =
            Dict.get "iyr" fields
                |> Maybe.andThen String.toInt
                |> Maybe.map (\v -> v >= 2010 && v <= 2020)
                |> Maybe.withDefault False

        eyr =
            Dict.get "eyr" fields
                |> Maybe.andThen String.toInt
                |> Maybe.map (\v -> v >= 2020 && v <= 2030)
                |> Maybe.withDefault False

        hgt =
            Dict.get "hgt" fields
                |> Maybe.andThen
                    (\s ->
                        if String.endsWith "cm" s then
                            String.replace "cm" "" s
                                |> String.toInt
                                |> Maybe.map (\v -> v >= 150 && v <= 193)

                        else
                            String.replace "in" "" s
                                |> String.toInt
                                |> Maybe.map (\v -> v >= 59 && v <= 76)
                    )
                |> Maybe.withDefault False

        hcl =
            Dict.get "hcl" fields
                |> Maybe.map (Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "^#[a-f0-9]{6}$"))
                |> Maybe.withDefault False

        ecl =
            Dict.get "ecl" fields
                |> Maybe.map (Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "^amb|blu|brn|gry|grn|hzl|oth$"))
                |> Maybe.withDefault False

        pid =
            Dict.get "pid" fields
                |> Maybe.map (Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "^\\d{9}$"))
                |> Maybe.withDefault False
    in
    byr && iyr && eyr && hgt && hcl && ecl && pid


solve2 : String
solve2 =
    Day4.Input.input
        |> String.split "\n\n"
        |> List.map
            (String.replace "\n" " "
                >> String.split " "
                >> List.map (String.split ":" >> listToTuple)
                >> Dict.fromList
            )
        |> List.filter validPassword
        |> List.length
        |> String.fromInt


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
