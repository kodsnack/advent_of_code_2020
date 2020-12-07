module Day2.Solver exposing (solve1, solve2)

import Day2.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Regex
import String.Extra


helper : (Int -> Int -> String -> String -> Bool) -> String
helper policy =
    Day2.Input.input
        |> String.split "\n"
        |> List.map (Regex.find (Maybe.withDefault Regex.never <| Regex.fromString "(\\d*)-(\\d*) (\\w): (\\w*)"))
        |> List.filter
            (\matches ->
                case List.concatMap .submatches matches of
                    (Just fromStr) :: (Just toStr) :: (Just char) :: (Just password) :: [] ->
                        let
                            from =
                                String.toInt fromStr |> Maybe.withDefault 0

                            to =
                                String.toInt toStr |> Maybe.withDefault 0
                        in
                        policy from to char password

                    _ ->
                        False
            )
        |> List.length
        |> String.fromInt


solve1 : String
solve1 =
    helper
        (\from to char password ->
            let
                count =
                    String.Extra.countOccurrences char password
            in
            count >= from && count <= to
        )


solve2 : String
solve2 =
    helper
        (\from to char password ->
            [ String.slice (from - 1) from password
            , String.slice (to - 1) to password
            ]
                |> List.map ((==) char)
                |> List.filter identity
                |> (List.length >> (==) 1)
        )


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
