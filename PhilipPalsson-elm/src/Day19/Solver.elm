module Day19.Solver exposing (solve1, solve2)

import Day19.Input
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Regex exposing (Match)


type Rule
    = Char String
    | RuleList (List (List String))


parseRule : String -> Maybe ( String, Rule )
parseRule string =
    case String.split ": " string of
        ruleId :: [ ruleString ] ->
            Just
                ( ruleId
                , if String.startsWith "\"" ruleString then
                    String.replace "\"" "" ruleString |> Char

                  else
                    String.split " | " ruleString
                        |> List.map (String.split " ")
                        |> RuleList
                )

        _ ->
            Nothing


ruleRegexp : Int -> Dict String Rule -> String -> String
ruleRegexp depth rules ruleId =
    if depth > 15 then
        ""

    else
        Dict.get ruleId rules
            |> Maybe.map
                (\rule ->
                    case rule of
                        Char string ->
                            string

                        RuleList subRules ->
                            let
                                subRulesRegexp =
                                    subRules
                                        |> List.map (List.map (ruleRegexp (depth + 1) rules) >> String.join "")
                                        |> String.join "|"
                            in
                            "(?:" ++ subRulesRegexp ++ ")"
                )
            |> Maybe.withDefault ""


solve customRules =
    case String.split "\n\n" Day19.Input.input of
        rulesString :: [ messagesString ] ->
            let
                rules =
                    Dict.fromList ((rulesString |> String.lines |> List.filterMap parseRule) ++ customRules)

                regexp =
                    Regex.fromString ("^" ++ ruleRegexp 0 rules "0" ++ "$")
                        |> Maybe.withDefault Regex.never
            in
            List.filter (Regex.contains regexp) (String.lines messagesString)
                |> List.length

        _ ->
            0


solve1 : Int
solve1 =
    solve []


solve2 : Int
solve2 =
    solve
        [ ( "8", RuleList [ [ "42" ], [ "42", "8" ] ] )
        , ( "11", RuleList [ [ "42", "31" ], [ "42", "11", "31" ] ] )
        ]


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ (solve1 |> String.fromInt)) ]
        , div [] [ text ("Solution part 2: " ++ (solve2 |> String.fromInt)) ]
        ]
