module Day1.Solver exposing (solve1, solve2)

import Day1.Input
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


helper1 : List number -> List number
helper1 list =
    List.concatMap
        (\x ->
            List.filterMap
                (\y ->
                    if x + y == 2020 then
                        Just <| x * y

                    else
                        Nothing
                )
                list
        )
        list


helper2 : List number -> List number
helper2 list =
    List.concatMap
        (\x ->
            List.concatMap
                (\y ->
                    List.filterMap
                        (\z ->
                            if x + y + z == 2020 then
                                Just <| x * y * z

                            else
                                Nothing
                        )
                        list
                )
                list
        )
        list


solve1 : String
solve1 =
    Day1.Input.input
        |> String.split "\n"
        |> List.filterMap String.toInt
        |> helper1
        |> List.map String.fromInt
        |> List.head
        |> Maybe.withDefault ""


solve2 : String
solve2 =
    Day1.Input.input
        |> String.split "\n"
        |> List.filterMap String.toInt
        |> helper2
        |> List.map String.fromInt
        |> List.head
        |> Maybe.withDefault ""


main : Html msg
main =
    div [ style "text-align" "center" ]
        [ div [] [ text ("Solution part 1: " ++ solve1) ]
        , div [] [ text ("Solution part 2: " ++ solve2) ]
        ]
